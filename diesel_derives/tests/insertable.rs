use crate::helpers::*;
use crate::schema::*;
use diesel::*;

#[test]
fn simple_struct_definition() {
    #[derive(Insertable)]
    #[diesel(table_name = users)]
    struct NewUser {
        name: String,
        hair_color: String,
    }

    let conn = &mut connection();
    let new_user = NewUser {
        name: "Sean".into(),
        hair_color: "Black".into(),
    };
    insert_into(users::table)
        .values(new_user)
        .execute(conn)
        .unwrap();

    let saved = users::table
        .select((users::name, users::hair_color))
        .load::<(String, Option<String>)>(conn);
    let expected = vec![("Sean".to_string(), Some("Black".to_string()))];
    assert_eq!(Ok(expected), saved);
}

#[test]
fn with_implicit_table_name() {
    #[derive(Insertable)]
    struct User {
        name: String,
        hair_color: String,
    }

    let conn = &mut connection();
    let new_user = User {
        name: "Sean".into(),
        hair_color: "Black".into(),
    };
    insert_into(users::table)
        .values(new_user)
        .execute(conn)
        .unwrap();

    let saved = users::table
        .select((users::name, users::hair_color))
        .load::<(String, Option<String>)>(conn);
    let expected = vec![("Sean".to_string(), Some("Black".to_string()))];
    assert_eq!(Ok(expected), saved);
}

#[test]
fn with_path_in_table_name() {
    #[derive(Insertable)]
    #[diesel(table_name = crate::schema::users)]
    struct NewUser {
        name: String,
        hair_color: String,
    }

    let conn = &mut connection();
    let new_user = NewUser {
        name: "Sean".into(),
        hair_color: "Black".into(),
    };
    insert_into(users::table)
        .values(new_user)
        .execute(conn)
        .unwrap();

    let saved = users::table
        .select((users::name, users::hair_color))
        .load::<(String, Option<String>)>(conn);
    let expected = vec![("Sean".to_string(), Some("Black".to_string()))];
    assert_eq!(Ok(expected), saved);
}

#[test]
fn simple_reference_definition() {
    #[derive(Insertable)]
    #[diesel(table_name = users)]
    struct NewUser {
        name: String,
        hair_color: String,
    }

    let conn = &mut connection();
    let new_user = NewUser {
        name: "Sean".into(),
        hair_color: "Black".into(),
    };
    insert_into(users::table)
        .values(&new_user)
        .execute(conn)
        .unwrap();

    let saved = users::table
        .select((users::name, users::hair_color))
        .load::<(String, Option<String>)>(conn);
    let expected = vec![("Sean".to_string(), Some("Black".to_string()))];
    assert_eq!(Ok(expected), saved);
}

#[test]
fn multiple_tables() {
    #[derive(Clone, Insertable)]
    #[diesel(table_name = users)]
    #[diesel(table_name = users_)]
    struct NewUser {
        name: String,
        hair_color: String,
    }

    let conn = &mut connection();
    let new_user = NewUser {
        name: "Sean".into(),
        hair_color: "Black".into(),
    };
    insert_into(users::table)
        .values(new_user.clone())
        .execute(conn)
        .unwrap();

    let saved = users::table
        .select((users::name, users::hair_color))
        .load::<(String, Option<String>)>(conn);
    let expected = vec![("Sean".to_string(), Some("Black".to_string()))];
    assert_eq!(Ok(expected.clone()), saved);

    insert_into(users_::table)
        .values(new_user)
        .execute(conn)
        .unwrap();

    let saved = users_::table
        .select((users_::name, users_::hair_color))
        .load::<(String, Option<String>)>(conn);
    assert_eq!(Ok(expected), saved);
}

macro_rules! test_struct_definition {
    ($test_name:ident, $struct_def:item) => {
        #[test]
        fn $test_name() {
            #[derive(Insertable)]
            #[diesel(table_name = users)]
            $struct_def

            let conn = &mut connection();
            let new_user = NewUser { name: "Sean".into(), hair_color: None };
            insert_into(users::table).values(&new_user).execute(conn).unwrap();

            let saved = users::table.select((users::name, users::hair_color))
                .load::<(String, Option<String>)>(conn);
            let expected = vec![("Sean".to_string(), Some("Green".to_string()))];
            assert_eq!(Ok(expected), saved);
        }
    }
}

test_struct_definition! {
    struct_with_option_field,
    struct NewUser {
        name: String,
        hair_color: Option<String>,
    }
}

test_struct_definition! {
    pub_struct_definition,
    pub struct NewUser {
        name: String,
        hair_color: Option<String>,
    }
}

test_struct_definition! {
    struct_with_pub_field,
    pub struct NewUser {
        pub name: String,
        hair_color: Option<String>,
    }
}

test_struct_definition! {
    struct_with_pub_option_field,
    pub struct NewUser {
        name: String,
        pub hair_color: Option<String>,
    }
}

test_struct_definition! {
    named_struct_with_borrowed_body,
    struct NewUser<'a> {
        name: &'a str,
        hair_color: Option<&'a str>,
    }
}

#[test]
fn named_struct_with_renamed_field() {
    #[derive(Insertable)]
    #[diesel(table_name = users)]
    struct NewUser {
        #[diesel(column_name = name)]
        my_name: String,
        hair_color: String,
    }

    let conn = &mut connection();
    let new_user = NewUser {
        my_name: "Sean".into(),
        hair_color: "Black".into(),
    };
    insert_into(users::table)
        .values(&new_user)
        .execute(conn)
        .unwrap();

    let saved = users::table
        .select((users::name, users::hair_color))
        .load::<(String, Option<String>)>(conn);
    let expected = vec![("Sean".to_string(), Some("Black".to_string()))];
    assert_eq!(Ok(expected), saved);
}

#[test]
fn named_struct_with_renamed_option_field() {
    #[derive(Insertable)]
    #[diesel(table_name = users)]
    struct NewUser {
        #[diesel(column_name = name)]
        my_name: String,
        #[diesel(column_name = hair_color)]
        my_hair_color: Option<String>,
    }

    let conn = &mut connection();
    let new_user = NewUser {
        my_name: "Sean".into(),
        my_hair_color: None,
    };
    insert_into(users::table)
        .values(&new_user)
        .execute(conn)
        .unwrap();

    let saved = users::table
        .select((users::name, users::hair_color))
        .load::<(String, Option<String>)>(conn);
    let expected = vec![("Sean".to_string(), Some("Green".to_string()))];
    assert_eq!(Ok(expected), saved);
}

#[test]
fn tuple_struct() {
    #[derive(Insertable)]
    #[diesel(table_name = users)]
    struct NewUser<'a>(
        #[diesel(column_name = name)] &'a str,
        #[diesel(column_name = hair_color)] Option<&'a str>,
    );

    let conn = &mut connection();
    let new_user = NewUser("Sean", None);
    insert_into(users::table)
        .values(&new_user)
        .execute(conn)
        .unwrap();

    let saved = users::table
        .select((users::name, users::hair_color))
        .load::<(String, Option<String>)>(conn);
    let expected = vec![("Sean".to_string(), Some("Green".to_string()))];
    assert_eq!(Ok(expected), saved);
}

#[test]
fn named_struct_with_unusual_reference_type() {
    #[derive(Insertable)]
    #[diesel(table_name = users)]
    struct NewUser<'a> {
        name: &'a String,
        hair_color: Option<&'a String>,
    }

    let conn = &mut connection();
    let sean = "Sean".to_string();
    let black = "Black".to_string();
    let new_user = NewUser {
        name: &sean,
        hair_color: Some(&black),
    };
    insert_into(users::table)
        .values(&new_user)
        .execute(conn)
        .unwrap();

    let saved = users::table
        .select((users::name, users::hair_color))
        .load(conn);
    let expected = vec![(sean.clone(), Some(black.clone()))];
    assert_eq!(Ok(expected), saved);
}

#[test]
#[cfg(all(feature = "postgres", not(feature = "sqlite")))]
fn insertable_with_slice_of_borrowed() {
    table! {
        posts {
            id -> Serial,
            tags -> Array<Text>,
        }
    }

    #[derive(Insertable)]
    #[diesel(table_name = posts)]
    struct NewPost<'a> {
        tags: &'a [&'a str],
    }

    let conn = &mut connection();
    sql_query("DROP TABLE IF EXISTS posts CASCADE")
        .execute(conn)
        .unwrap();
    sql_query("CREATE TABLE posts (id SERIAL PRIMARY KEY, tags TEXT[] NOT NULL)")
        .execute(conn)
        .unwrap();
    let new_post = NewPost {
        tags: &["hi", "there"],
    };
    insert_into(posts::table)
        .values(&new_post)
        .execute(conn)
        .unwrap();

    let saved = posts::table.select(posts::tags).load::<Vec<String>>(conn);
    let expected = vec![vec![String::from("hi"), String::from("there")]];
    assert_eq!(Ok(expected), saved);
}

#[test]
fn embedded_struct() {
    #[derive(Insertable)]
    #[diesel(table_name = users)]
    struct NameAndHairColor<'a> {
        name: &'a str,
        hair_color: &'a str,
    }

    #[derive(Insertable)]
    struct User<'a> {
        id: i32,
        #[diesel(embed)]
        name_and_hair_color: NameAndHairColor<'a>,
    }

    let conn = &mut connection();
    let new_user = User {
        id: 1,
        name_and_hair_color: NameAndHairColor {
            name: "Sean",
            hair_color: "Black",
        },
    };
    insert_into(users::table)
        .values(&new_user)
        .execute(conn)
        .unwrap();

    let saved = users::table.load::<(i32, String, Option<String>)>(conn);
    let expected = vec![(1, "Sean".to_string(), Some("Black".to_string()))];
    assert_eq!(Ok(expected), saved);
}

#[test]
fn serialize_fn_custom_option_field_closure() {
    struct UserName(String);
    impl From<UserName> for String {
        fn from(value: UserName) -> Self {
            value.0
        }
    }

    enum HairColor {
        Green,
    }

    impl From<HairColor> for String {
        fn from(value: HairColor) -> Self {
            match value {
                HairColor::Green => "Green".into(),
            }
        }
    }

    #[derive(Insertable)]
    #[diesel(table_name = users)]
    #[diesel(treat_none_as_default_value = false)]
    struct NewUser {
        #[diesel(serialize_as = String)]
        name: UserName,
        #[diesel(serialize_as = Option<String>)]
        #[diesel(serialize_fn = |x: Option<HairColor>| x.map(Into::into))]
        hair_color: Option<HairColor>,
    }

    let conn = &mut connection();
    let new_user = NewUser {
        name: UserName("Sean".into()),
        hair_color: Some(HairColor::Green),
    };
    insert_into(users::table)
        .values(new_user)
        .execute(conn)
        .unwrap();

    let saved = users::table
        .select((users::name, users::hair_color))
        .load::<(String, Option<String>)>(conn);
    let expected = vec![("Sean".to_string(), Some("Green".to_string()))];
    assert_eq!(Ok(expected), saved);
}

#[test]
fn serialize_fn_custom_option_field_function() {
    struct UserName(String);
    impl From<UserName> for String {
        fn from(value: UserName) -> Self {
            value.0
        }
    }

    enum HairColor {
        Green,
    }

    fn hair_color_to_string(value: Option<HairColor>) -> Option<String> {
        value.map(|value| match value {
            HairColor::Green => "Green".into(),
        })
    }

    #[derive(Insertable)]
    #[diesel(table_name = users)]
    #[diesel(treat_none_as_default_value = false)]
    struct NewUser {
        #[diesel(serialize_as = String)]
        name: UserName,
        #[diesel(serialize_as = Option<String>)]
        #[diesel(serialize_fn = hair_color_to_string)]
        hair_color: Option<HairColor>,
    }

    let conn = &mut connection();
    let new_user = NewUser {
        name: UserName("Sean".into()),
        hair_color: Some(HairColor::Green),
    };
    insert_into(users::table)
        .values(new_user)
        .execute(conn)
        .unwrap();

    let saved = users::table
        .select((users::name, users::hair_color))
        .load::<(String, Option<String>)>(conn);
    let expected = vec![("Sean".to_string(), Some("Green".to_string()))];
    assert_eq!(Ok(expected), saved);
}

#[test]
fn serialize_fn_custom_option_field_associated_function() {
    struct UserName(String);
    impl From<UserName> for String {
        fn from(value: UserName) -> Self {
            value.0
        }
    }

    enum HairColor {
        Green,
    }

    impl HairColor {
        fn to_string(value: Option<Self>) -> Option<String> {
            value.map(|value| match value {
                HairColor::Green => "Green".into(),
            })
        }
    }

    #[derive(Insertable)]
    #[diesel(table_name = users)]
    #[diesel(treat_none_as_default_value = false)]
    struct NewUser {
        #[diesel(serialize_as = String)]
        name: UserName,
        #[diesel(serialize_as = Option<String>)]
        #[diesel(serialize_fn = HairColor::to_string)]
        hair_color: Option<HairColor>,
    }

    let conn = &mut connection();
    let new_user = NewUser {
        name: UserName("Sean".into()),
        hair_color: Some(HairColor::Green),
    };
    insert_into(users::table)
        .values(new_user)
        .execute(conn)
        .unwrap();

    let saved = users::table
        .select((users::name, users::hair_color))
        .load::<(String, Option<String>)>(conn);
    let expected = vec![("Sean".to_string(), Some("Green".to_string()))];
    assert_eq!(Ok(expected), saved);
}

#[test]
fn serialize_fn_overrides_from() {
    struct UserName(String);
    impl From<UserName> for String {
        fn from(value: UserName) -> Self {
            value.0
        }
    }

    enum HairColor {
        Green,
    }

    impl From<HairColor> for String {
        fn from(value: HairColor) -> Self {
            match value {
                HairColor::Green => "error".into(),
            }
        }
    }

    fn hair_color_to_string(value: HairColor) -> String {
        match value {
            HairColor::Green => "Green".into(),
        }
    }

    #[derive(Insertable)]
    #[diesel(table_name = users)]
    #[diesel(treat_none_as_default_value = false)]
    struct NewUser {
        #[diesel(serialize_as = String)]
        name: UserName,
        #[diesel(serialize_as = String)]
        #[diesel(serialize_fn = hair_color_to_string)]
        hair_color: HairColor,
    }

    let conn = &mut connection();
    let new_user = NewUser {
        name: UserName("Sean".into()),
        hair_color: HairColor::Green,
    };
    insert_into(users::table)
        .values(new_user)
        .execute(conn)
        .unwrap();

    let saved = users::table
        .select((users::name, users::hair_color))
        .load::<(String, Option<String>)>(conn);
    let expected = vec![("Sean".to_string(), Some("Green".to_string()))];
    assert_eq!(Ok(expected), saved);
}
