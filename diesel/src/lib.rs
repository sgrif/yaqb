//! Diesel is an ORM and query builder designed to reduce the boilerplate for database
//! interactions. [A getting started guide](https://github.com/sgrif/diesel#getting-started) can be
//! found in the README.
#![deny(warnings)]
#![cfg_attr(feature = "unstable", feature(time2))]
pub mod backend;
pub mod connection;
pub mod expression;
#[doc(hidden)]
pub mod persistable;
pub mod types;

#[macro_use]
mod macros;

mod db_result;
pub mod migrations;
pub mod query_builder;
mod query_dsl;
pub mod query_source;
pub mod result;
#[doc(hidden)]
pub mod row;

pub mod helper_types {
    //! Provide helper types for concisely writing the return type of functions.
    //! As with iterators, it is unfortunately difficult to return a partially
    //! constructed query without exposing the exact implementation of the
    //! function. Without higher kinded types, these various DSLs can't be
    //! combined into a single trait for boxing purposes.
    //!
    //! All types here are in the form `<FirstType as
    //! DslName<OtherTypes>>::Output`. So the return type of
    //! `users.filter(first_name.eq("John")).order(last_name.asc()).limit(10)` would
    //! be `Limit<Order<FindBy<users, first_name, &str>, Asc<last_name>>>`
    use super::query_dsl::*;
    use super::expression::helper_types::Eq;

    /// Represents the return type of `.select(selection)`
    pub type Select<Source, Selection, Type = <Selection as super::Expression>::SqlType> =
        <Source as SelectDsl<Selection, Type>>::Output;

    /// Represents the return type of `.filter(predicate)`
    pub type Filter<Source, Predicate> =
        <Source as FilterDsl<Predicate>>::Output;

    /// Represents the return type of `.filter(lhs.eq(rhs))`
    pub type FindBy<Source, Column, Value> =
        Filter<Source, Eq<Column, Value>>;

    /// Represents the return type of `.order(ordering)`
    pub type Order<Source, Ordering> =
        <Source as OrderDsl<Ordering>>::Output;

    /// Represents the return type of `.limit()`
    pub type Limit<Source> = <Source as LimitDsl>::Output;

    /// Represents the return type of `.offset()`
    pub type Offset<Source> = <Source as OffsetDsl>::Output;

    /// Represents the return type of `.with(aliased_expr)`
    pub type With<'a, Source, Other> = <Source as WithDsl<'a, Other>>::Output;
}

pub mod prelude {
    //! Re-exports important traits and types. Meant to be glob imported when using Diesel.
    pub use connection::Connection;
    pub use expression::{Expression, SelectableExpression, BoxableExpression};
    pub use expression::expression_methods::*;
    #[doc(inline)]
    pub use persistable::Insertable;
    pub use query_dsl::*;
    pub use query_source::{QuerySource, Queryable, Table, Column, JoinTo};
    pub use result::{QueryResult, TransactionError, TransactionResult, ConnectionError, ConnectionResult, OptionalExtension};
}

pub use prelude::*;
#[doc(inline)]
pub use query_builder::functions::{insert, update, delete, select};
pub use result::Error::NotFound;
#[doc(inline)]
pub use types::structs::data_types;
