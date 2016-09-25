use expression::Expression;
use super::{IntoUpdateTarget, IncompleteUpdateStatement, IncompleteInsertStatement, SelectStatement};
use super::delete_statement::DeleteStatement;
use super::insert_statement::Insert;

/// Creates an update statement. Helpers for updating a single row can be
/// generated by deriving [`AsChangeset`][trait.AsChangeset.html]
///
/// # Examples
///
/// ### Updating a single record:
///
/// ```rust
/// # #[macro_use] extern crate diesel;
/// # include!("src/doctest_setup.rs");
/// #
/// # table! {
/// #     users {
/// #         id -> Integer,
/// #         name -> VarChar,
/// #     }
/// # }
/// #
/// # #[cfg(feature = "postgres")]
/// # fn main() {
/// #     use self::users::dsl::*;
/// #     let connection = establish_connection();
/// let updated_row = diesel::update(users.filter(id.eq(1)))
///     .set(name.eq("James"))
///     .get_result(&connection);
/// // On backends that support it, you can call `get_result` instead of `execute`
/// // to have `RETURNING *` automatically appended to the query. Alternatively, you
/// // can explicitly return an expression by using the `returning` method before
/// // getting the result.
/// assert_eq!(Ok((1, "James".to_string())), updated_row);
/// # }
/// # #[cfg(not(feature = "postgres"))]
/// # fn main() {}
/// ```
pub fn update<T: IntoUpdateTarget>(source: T) -> IncompleteUpdateStatement<T::Table, T::WhereClause> {
    IncompleteUpdateStatement::new(source.into_update_target())
}

/// Creates a delete statement. Will delete the records in the given set.
/// Because this function has a very generic name, it is not exported by
/// default.
///
/// # Examples
///
/// ### Deleting a single record:
///
/// ```rust
/// # #[macro_use] extern crate diesel;
/// # include!("src/doctest_setup.rs");
/// #
/// # table! {
/// #     users {
/// #         id -> Integer,
/// #         name -> VarChar,
/// #     }
/// # }
/// #
/// # fn main() {
/// #     delete();
/// # }
/// #
/// # fn delete() -> QueryResult<()> {
/// #     use self::users::dsl::*;
/// #     let connection = establish_connection();
/// #     let get_count = || users.count().first::<i64>(&connection);
/// let old_count = get_count();
/// try!(diesel::delete(users.filter(id.eq(1))).execute(&connection));
/// assert_eq!(old_count.map(|count| count - 1), get_count());
/// # Ok(())
/// # }
/// ```
///
/// ### Deleting a whole table:
///
/// ```rust
/// # #[macro_use] extern crate diesel;
/// # include!("src/doctest_setup.rs");
/// #
/// # table! {
/// #     users {
/// #         id -> Integer,
/// #         name -> VarChar,
/// #     }
/// # }
/// #
/// # fn main() {
/// #     delete();
/// # }
/// #
/// # fn delete() -> QueryResult<()> {
/// #     use self::users::dsl::*;
/// #     let connection = establish_connection();
/// #     let get_count = || users.count().first::<i64>(&connection);
/// try!(diesel::delete(users).execute(&connection));
/// assert_eq!(Ok(0), get_count());
/// # Ok(())
/// # }
/// ```
pub fn delete<T: IntoUpdateTarget>(source: T) -> DeleteStatement<T::Table, T::WhereClause> {
    DeleteStatement::new(source.into_update_target())
}

/// Creates an insert statement. Will add the given data to a table. This
/// function is not exported by default. As with other commands, the resulting
/// query can return the inserted rows if you choose.
pub fn insert<'a, T: ?Sized>(records: &'a T) -> IncompleteInsertStatement<&'a T, Insert> {
    IncompleteInsertStatement::new(records, Insert)
}

/// Creates a bare select statement, with no from clause. Primarily used for
/// testing diesel itself, but likely useful for third party crates as well. The
/// given expressions must be selectable from anywhere.
pub fn select<T>(expression: T) -> SelectStatement<T::SqlType, T, ()> where
    T: Expression,
{
    SelectStatement::simple(expression, ())
}
