#[path = "common.rs"]
mod common;

use common::*;
use super::Bencher;
use diesel::*;
use diesel_async::AsyncConnection;
use diesel_async::RunQueryDsl;
use tokio::runtime::Runtime;

#[cfg(feature = "postgres")]
type TestConnection = diesel_async::AsyncPgConnection;

#[cfg(feature = "mysql")]
type TestConnection = diesel_async::AsyncMysqlConnection;

#[derive(Debug, PartialEq, Eq, Queryable, Clone, Insertable, AsChangeset)]
#[diesel(table_name = users)]
#[diesel(treat_none_as_default_value = false)]
pub struct NewUser {
    pub name: String,
    pub hair_color: Option<String>,
}

impl NewUser {
    pub fn new(name: &str, hair_color: Option<&str>) -> Self {
        NewUser {
            name: name.to_string(),
            hair_color: hair_color.map(|s| s.to_string()),
        }
    }
}

#[derive(Insertable)]
#[diesel(table_name = posts)]
pub struct NewPost {
    user_id: i32,
    title: String,
    body: Option<String>,
}

impl NewPost {
    pub fn new(user_id: i32, title: &str, body: Option<&str>) -> Self {
        NewPost {
            user_id,
            title: title.into(),
            body: body.map(|b| b.into()),
        }
    }
}

#[derive(Debug, Clone, Copy, Insertable)]
#[diesel(table_name = comments)]
pub struct NewComment<'a>(
    #[diesel(column_name = post_id)] pub i32,
    #[diesel(column_name = text)] pub &'a str,
);

#[cfg(feature = "mysql")]
async fn connection() -> TestConnection {
    dotenvy::dotenv().ok();
    let connection_url = dotenvy::var("MYSQL_DATABASE_URL")
        .or_else(|_| dotenvy::var("DATABASE_URL"))
        .expect("DATABASE_URL must be set in order to run tests");
    let mut conn = diesel_async::AsyncMysqlConnection::establish(&connection_url)
        .await
        .unwrap();
    diesel::sql_query("SET FOREIGN_KEY_CHECKS = 0;")
        .execute(&mut conn)
        .await
        .unwrap();
    diesel::sql_query("TRUNCATE TABLE comments")
        .execute(&mut conn)
        .await
        .unwrap();
    diesel::sql_query("TRUNCATE TABLE posts")
        .execute(&mut conn)
        .await
        .unwrap();
    diesel::sql_query("TRUNCATE TABLE users")
        .execute(&mut conn)
        .await
        .unwrap();
    diesel::sql_query("SET FOREIGN_KEY_CHECKS = 1;")
        .execute(&mut conn)
        .await
        .unwrap();
    conn
}

#[cfg(feature = "postgres")]
async fn connection() -> TestConnection {
    dotenvy::dotenv().ok();
    let connection_url = dotenvy::var("PG_DATABASE_URL")
        .or_else(|_| dotenvy::var("DATABASE_URL"))
        .expect("DATABASE_URL must be set in order to run tests");
    let mut conn = diesel_async::AsyncPgConnection::establish(&connection_url)
        .await
        .unwrap();
    diesel::sql_query("TRUNCATE TABLE comments CASCADE")
        .execute(&mut conn)
        .await
        .unwrap();
    diesel::sql_query("TRUNCATE TABLE posts CASCADE")
        .execute(&mut conn)
        .await
        .unwrap();
    diesel::sql_query("TRUNCATE TABLE users CASCADE")
        .execute(&mut conn)
        .await
        .unwrap();
    conn
}

async fn insert_users<F: Fn(usize) -> Option<&'static str>, const N: usize>(
    conn: &mut TestConnection,
    hair_color_init: F,
) {
    const DUMMY_USER: NewUser = NewUser {
        name: String::new(),
        hair_color: None,
    };

    // There are stackoverflows on windows otherwise
    if N > 1_000 {
        let mut data = Box::new([DUMMY_USER; N]);

        for (idx, user) in data.iter_mut().enumerate() {
            *user = NewUser::new(&format!("User {}", idx), hair_color_init(idx));
        }

        insert_into(users::table)
            .values(data)
            .execute(conn)
            .await
            .unwrap();
    } else {
        let mut data = [DUMMY_USER; N];

        for (idx, user) in data.iter_mut().enumerate() {
            *user = NewUser::new(&format!("User {}", idx), hair_color_init(idx));
        }

        insert_into(users::table)
            .values(data)
            .execute(conn)
            .await
            .unwrap();
    }
}

pub fn bench_trivial_query(b: &mut Bencher, size: usize) {
    let runtime = Runtime::new().expect("Failed to create runtime");
    let mut conn = runtime.block_on(async {
        let mut conn = connection().await;
        match size {
            1 => insert_users::<_, 1>(&mut conn, |_| None).await,
            10 => insert_users::<_, 10>(&mut conn, |_| None).await,
            100 => insert_users::<_, 100>(&mut conn, |_| None).await,
            1_000 => insert_users::<_, 1_000>(&mut conn, |_| None).await,
            10_000 => insert_users::<_, 10_000>(&mut conn, |_| None).await,
            _ => unimplemented!(),
        };
        conn
    });

    b.iter(|| runtime.block_on(async { users::table.load::<User>(&mut conn).await.unwrap() }))
}

pub fn bench_trivial_query_boxed(b: &mut Bencher, size: usize) {
    let runtime = Runtime::new().expect("Failed to create runtime");
    let mut conn = runtime.block_on(async {
        let mut conn = connection().await;

        match size {
            1 => insert_users::<_, 1>(&mut conn, |_| None).await,
            10 => insert_users::<_, 10>(&mut conn, |_| None).await,
            100 => insert_users::<_, 100>(&mut conn, |_| None).await,
            1_000 => insert_users::<_, 1_000>(&mut conn, |_| None).await,
            10_000 => insert_users::<_, 10_000>(&mut conn, |_| None).await,
            _ => unimplemented!(),
        };
        conn
    });
    b.iter(|| {
        runtime.block_on(async {
            users::table
                .into_boxed()
                .load::<User>(&mut conn)
                .await
                .unwrap()
        })
    })
}

pub fn bench_trivial_query_raw(b: &mut Bencher, size: usize) {
    let runtime = Runtime::new().expect("Failed to create runtime");
    let mut conn = runtime.block_on(async {
        let mut conn = connection().await;
        match size {
            1 => insert_users::<_, 1>(&mut conn, |_| None).await,
            10 => insert_users::<_, 10>(&mut conn, |_| None).await,
            100 => insert_users::<_, 100>(&mut conn, |_| None).await,
            1_000 => insert_users::<_, 1_000>(&mut conn, |_| None).await,
            10_000 => insert_users::<_, 10_000>(&mut conn, |_| None).await,
            _ => unimplemented!(),
        }
        conn
    });
    b.iter(|| {
        runtime.block_on(async {
            diesel::sql_query("SELECT id, name, hair_color FROM users")
                .load::<User>(&mut conn)
                .await
                .unwrap()
        })
    })
}

pub fn bench_medium_complex_query(b: &mut Bencher, size: usize) {
    let runtime = Runtime::new().expect("Failed to create runtime");
    let mut conn = runtime.block_on(async {
        let mut conn = connection().await;
        let hair_color_callback = |i| Some(if i % 2 == 0 { "black" } else { "brown" });
        match size {
            1 => insert_users::<_, 1>(&mut conn, hair_color_callback).await,
            10 => insert_users::<_, 10>(&mut conn, hair_color_callback).await,
            100 => insert_users::<_, 100>(&mut conn, hair_color_callback).await,
            1_000 => insert_users::<_, 1_000>(&mut conn, hair_color_callback).await,
            10_000 => insert_users::<_, 10_000>(&mut conn, hair_color_callback).await,
            _ => unimplemented!(),
        }
        conn
    });

    b.iter(|| {
        runtime.block_on(async {
            use self::users::dsl::*;
            let target = users
                .left_outer_join(posts::table)
                .filter(hair_color.eq("black"));
            target
                .load::<(User, Option<Post>)>(&mut conn)
                .await
                .unwrap()
        })
    })
}

pub fn bench_medium_complex_query_boxed(b: &mut Bencher, size: usize) {
    let runtime = Runtime::new().expect("Failed to create runtime");
    let mut conn = runtime.block_on(async {
        let mut conn = connection().await;
        let hair_color_callback = |i| Some(if i % 2 == 0 { "black" } else { "brown" });
        match size {
            1 => insert_users::<_, 1>(&mut conn, hair_color_callback).await,
            10 => insert_users::<_, 10>(&mut conn, hair_color_callback).await,
            100 => insert_users::<_, 100>(&mut conn, hair_color_callback).await,
            1_000 => insert_users::<_, 1_000>(&mut conn, hair_color_callback).await,
            10_000 => insert_users::<_, 10_000>(&mut conn, hair_color_callback).await,
            _ => unimplemented!(),
        }
        conn
    });

    b.iter(|| {
        runtime.block_on(async {
            use self::users::dsl::*;
            let target = users
                .left_outer_join(posts::table)
                .filter(hair_color.eq("black"))
                .into_boxed();
            target
                .load::<(User, Option<Post>)>(&mut conn)
                .await
                .unwrap()
        })
    })
}

pub fn bench_medium_complex_query_queryable_by_name(b: &mut Bencher, size: usize) {
    let runtime = Runtime::new().expect("Failed to create runtime");
    let mut conn = runtime.block_on(async {
        let mut conn = connection().await;

        let hair_color_callback = |i| Some(if i % 2 == 0 { "black" } else { "brown" });
        match size {
            1 => insert_users::<_, 1>(&mut conn, hair_color_callback).await,
            10 => insert_users::<_, 10>(&mut conn, hair_color_callback).await,
            100 => insert_users::<_, 100>(&mut conn, hair_color_callback).await,
            1_000 => insert_users::<_, 1_000>(&mut conn, hair_color_callback).await,
            10_000 => insert_users::<_, 10_000>(&mut conn, hair_color_callback).await,
            _ => unimplemented!(),
        }
        conn
    });
    #[cfg(feature = "postgres")]
    let bind = "$1";
    #[cfg(feature = "mysql")]
    let bind = "?";

    let query = format!(
        "SELECT u.id, u.name, u.hair_color, p.id, p.user_id, p.title, p.body \
         FROM users as u LEFT JOIN posts as p on u.id = p.user_id WHERE u.hair_color = {bind}"
    );
    let query = &query;
    b.iter(|| {
        runtime.block_on(async {
            diesel::sql_query(query)
                .bind::<diesel::sql_types::Text, _>("black")
                .load::<(User, Option<Post>)>(&mut conn)
                .await
                .unwrap()
        })
    })
}

pub fn bench_insert(b: &mut Bencher, size: usize) {
    let runtime = Runtime::new().expect("Failed to create runtime");
    let mut conn = runtime.block_on(connection());

    #[inline(always)]
    fn hair_color_callback(_: usize) -> Option<&'static str> {
        Some("hair_color")
    }

    match size {
        1 => b.iter(|| runtime.block_on(insert_users::<_, 1>(&mut conn, hair_color_callback))),
        10 => b.iter(|| runtime.block_on(insert_users::<_, 10>(&mut conn, hair_color_callback))),
        25 => b.iter(|| runtime.block_on(insert_users::<_, 25>(&mut conn, hair_color_callback))),
        50 => b.iter(|| runtime.block_on(insert_users::<_, 50>(&mut conn, hair_color_callback))),
        100 => b.iter(|| runtime.block_on(insert_users::<_, 100>(&mut conn, hair_color_callback))),
        _ => unimplemented!(),
    }
}

pub fn loading_associations_sequentially(b: &mut Bencher) {
    #[cfg(feature = "sqlite")]
    const USER_NUMBER: usize = 9;

    #[cfg(not(feature = "sqlite"))]
    const USER_NUMBER: usize = 100;

    let runtime = Runtime::new().expect("Failed to create runtime");

    let mut conn = runtime.block_on(async {
        // SETUP A TON OF DATA
        let mut conn = connection().await;

        insert_users::<_, USER_NUMBER>(&mut conn, |i| {
            Some(if i % 2 == 0 { "black" } else { "brown" })
        })
        .await;

        let all_users = users::table.load::<User>(&mut conn).await.unwrap();
        let data: Vec<_> = all_users
            .iter()
            .flat_map(|user| {
                let user_id = user.id;
                (0..10).map(move |i| {
                    let title = format!("Post {} by user {}", i, user_id);
                    NewPost::new(user_id, &title, None)
                })
            })
            .collect();
        insert_into(posts::table)
            .values(&data)
            .execute(&mut conn)
            .await
            .unwrap();
        let all_posts = posts::table.load::<Post>(&mut conn).await.unwrap();
        let data: Vec<_> = all_posts
            .iter()
            .flat_map(|post| {
                let post_id = post.id;
                (0..10).map(move |i| {
                    let title = format!("Comment {} on post {}", i, post_id);
                    (title, post_id)
                })
            })
            .collect();
        let comment_data: Vec<_> = data
            .iter()
            .map(|&(ref title, post_id)| NewComment(post_id, &title))
            .collect();
        insert_into(comments::table)
            .values(&comment_data)
            .execute(&mut conn)
            .await
            .unwrap();
        conn
    });

    // ACTUAL BENCHMARK
    b.iter(|| {
        runtime.block_on(async {
            let users = users::table.load::<User>(&mut conn).await.unwrap();
            let posts = Post::belonging_to(&users)
                .load::<Post>(&mut conn)
                .await
                .unwrap();
            let comments = Comment::belonging_to(&posts)
                .load::<Comment>(&mut conn)
                .await
                .unwrap()
                .grouped_by(&posts);
            let posts_and_comments = posts.into_iter().zip(comments).grouped_by(&users);
            users
                .into_iter()
                .zip(posts_and_comments)
                .collect::<Vec<(User, Vec<(Post, Vec<Comment>)>)>>()
        })
    })
}
