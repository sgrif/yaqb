use std::io::Write;
use std::ops::Add;

use deserialize::{self, FromSql};
use pg::{Pg, PgValue};
use serialize::{self, IsNull, Output, ToSql};
use sql_types::{self, Date, Interval, Time, Timestamp, Timestamptz}; 

#[cfg(feature = "chrono")]
mod chrono;
#[cfg(feature = "deprecated-time")]
mod deprecated_time;
#[cfg(feature = "quickcheck")]
mod quickcheck_impls;
mod std_time;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, FromSqlRow, AsExpression)]
#[sql_type = "Timestamp"]
#[sql_type = "Timestamptz"]
/// Timestamps are represented in Postgres as a 64 bit signed integer representing the number of
/// microseconds since January 1st 2000. This struct is a dumb wrapper type, meant only to indicate
/// the integer's meaning.
pub struct PgTimestamp(pub i64);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, FromSqlRow, AsExpression)]
#[sql_type = "Date"]
/// Dates are represented in Postgres as a 32 bit signed integer representing the number of julian
/// days since January 1st 2000. This struct is a dumb wrapper type, meant only to indicate the
/// integer's meaning.
pub struct PgDate(pub i32);

/// Time is represented in Postgres as a 64 bit signed integer representing the number of
/// microseconds since midnight. This struct is a dumb wrapper type, meant only to indicate the
/// integer's meaning.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, FromSqlRow, AsExpression)]
#[sql_type = "Time"]
pub struct PgTime(pub i64);

/// Intervals in Postgres are separated into 3 parts. A 64 bit integer representing time in
/// microseconds, a 32 bit integer representing number of days, and a 32 bit integer
/// representing number of months. This struct is a dumb wrapper type, meant only to indicate the
/// meaning of these parts.
#[derive(Debug, Clone, Copy, PartialEq, Eq, FromSqlRow, AsExpression)]
#[sql_type = "Interval"]
pub struct PgInterval {
    /// The number of whole microseconds
    pub microseconds: i64,
    /// The number of whole days
    pub days: i32,
    /// The number of whole months
    pub months: i32,
}

impl PgInterval {
    /// Constructs a new `PgInterval`
    ///
    /// No conversion occurs on the arguments. It is valid to provide a number
    /// of microseconds greater than the longest possible day, or a number of
    /// days greater than the longest possible month, as it is impossible to say
    /// how many months are in "40 days" without knowing a precise date.
    pub fn new(microseconds: i64, days: i32, months: i32) -> Self {
        PgInterval {
            microseconds: microseconds,
            days: days,
            months: months,
        }
    }

    /// Equivalent to `new(microseconds, 0, 0)`
    pub fn from_microseconds(microseconds: i64) -> Self {
        Self::new(microseconds, 0, 0)
    }

    /// Equivalent to `new(0, days, 0)`
    pub fn from_days(days: i32) -> Self {
        Self::new(0, days, 0)
    }

    /// Equivalent to `new(0, 0, months)`
    pub fn from_months(months: i32) -> Self {
        Self::new(0, 0, months)
    }
}

impl ToSql<sql_types::Timestamp, Pg> for PgTimestamp {
    fn to_sql<W: Write>(&self, out: &mut Output<W, Pg>) -> serialize::Result {
        ToSql::<sql_types::BigInt, Pg>::to_sql(&self.0, out)
    }
}

impl FromSql<sql_types::Timestamp, Pg> for PgTimestamp {
    fn from_sql(bytes: Option<PgValue>) -> deserialize::Result<Self> {
        FromSql::<sql_types::BigInt, Pg>::from_sql(bytes).map(PgTimestamp)
    }
}

impl ToSql<sql_types::Timestamptz, Pg> for PgTimestamp {
    fn to_sql<W: Write>(&self, out: &mut Output<W, Pg>) -> serialize::Result {
        ToSql::<sql_types::Timestamp, Pg>::to_sql(self, out)
    }
}

impl FromSql<sql_types::Timestamptz, Pg> for PgTimestamp {
    fn from_sql(bytes: Option<PgValue>) -> deserialize::Result<Self> {
        <PgTimestamp as FromSql<sql_types::Timestamp, Pg>>::from_sql(bytes)
    }
}

impl ToSql<sql_types::Date, Pg> for PgDate {
    fn to_sql<W: Write>(&self, out: &mut Output<W, Pg>) -> serialize::Result {
        ToSql::<sql_types::Integer, Pg>::to_sql(&self.0, out)
    }
}

impl FromSql<sql_types::Date, Pg> for PgDate {
    fn from_sql(bytes: Option<PgValue>) -> deserialize::Result<Self> {
        FromSql::<sql_types::Integer, Pg>::from_sql(bytes).map(PgDate)
    }
}

impl ToSql<sql_types::Time, Pg> for PgTime {
    fn to_sql<W: Write>(&self, out: &mut Output<W, Pg>) -> serialize::Result {
        ToSql::<sql_types::BigInt, Pg>::to_sql(&self.0, out)
    }
}

impl FromSql<sql_types::Time, Pg> for PgTime {
    fn from_sql(bytes: Option<PgValue>) -> deserialize::Result<Self> {
        FromSql::<sql_types::BigInt, Pg>::from_sql(bytes).map(PgTime)
    }
}

impl ToSql<sql_types::Interval, Pg> for PgInterval {
    fn to_sql<W: Write>(&self, out: &mut Output<W, Pg>) -> serialize::Result {
        try!(ToSql::<sql_types::BigInt, Pg>::to_sql(
            &self.microseconds,
            out
        ));
        try!(ToSql::<sql_types::Integer, Pg>::to_sql(&self.days, out));
        try!(ToSql::<sql_types::Integer, Pg>::to_sql(&self.months, out));
        Ok(IsNull::No)
    }
}

impl FromSql<sql_types::Interval, Pg> for PgInterval {
    fn from_sql(value: Option<PgValue>) -> deserialize::Result<Self> {
        let value = not_none!(value);
        let bytes = value.bytes();
        Ok(PgInterval {
            microseconds: try!(FromSql::<sql_types::BigInt, Pg>::from_sql(Some(
                PgValue::with_oid(bytes[..8].as_ptr() as *mut u8, 8, 0) // TODO Find OID
            ))),
            days: try!(FromSql::<sql_types::Integer, Pg>::from_sql(Some(
                PgValue::with_oid(bytes[8..12].as_ptr() as *mut u8, 4, 0) // TODO Find OID
            ))),
            months: try!(FromSql::<sql_types::Integer, Pg>::from_sql(Some(
                PgValue::with_oid(bytes[12..16].as_ptr() as *mut u8, 4, 0) // TODO Find OID
            ))),
        })
    }
}

impl Add<PgInterval> for PgInterval {
    type Output = PgInterval;

    fn add(self, other: PgInterval) -> Self::Output {
        PgInterval {
            microseconds: self.microseconds + other.microseconds,
            days: self.days + other.days,
            months: self.months + other.months,
        }
    }
}
