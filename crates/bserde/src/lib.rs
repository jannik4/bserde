#![deny(rust_2018_idioms)]

use std::{
    borrow::Cow,
    collections::VecDeque,
    io::{ErrorKind, Read, Result, Write},
    marker::PhantomData,
    mem::{self, MaybeUninit},
    num::{
        NonZeroI128, NonZeroI16, NonZeroI32, NonZeroI64, NonZeroI8, NonZeroIsize, NonZeroU128,
        NonZeroU16, NonZeroU32, NonZeroU64, NonZeroU8, NonZeroUsize, Wrapping,
    },
    ops::{Range, RangeFrom, RangeFull, RangeInclusive, RangeTo, RangeToInclusive},
    sync::atomic::{
        AtomicBool, AtomicI16, AtomicI32, AtomicI64, AtomicI8, AtomicIsize, AtomicU16, AtomicU32,
        AtomicU64, AtomicU8, AtomicUsize, Ordering,
    },
};

#[cfg(feature = "derive")]
pub use bserde_derive::{DeserializeFromBytes, SerializeAsBytes};

pub trait SerializeAsBytes {
    fn serialize_ne<W: Write>(&self, write: &mut W) -> Result<()>;
    fn serialize_le<W: Write>(&self, write: &mut W) -> Result<()>;
    fn serialize_be<W: Write>(&self, write: &mut W) -> Result<()>;

    fn serialize_ne_to_vec(&self) -> Vec<u8> {
        let mut buf = Vec::new();
        self.serialize_ne(&mut buf).unwrap();
        buf
    }
    fn serialize_le_to_vec(&self) -> Vec<u8> {
        let mut buf = Vec::new();
        self.serialize_le(&mut buf).unwrap();
        buf
    }
    fn serialize_be_to_vec(&self) -> Vec<u8> {
        let mut buf = Vec::new();
        self.serialize_be(&mut buf).unwrap();
        buf
    }
}

pub trait DeserializeFromBytes: Sized {
    fn deserialize_ne<R: Read>(read: &mut R) -> Result<Self>;
    fn deserialize_le<R: Read>(read: &mut R) -> Result<Self>;
    fn deserialize_be<R: Read>(read: &mut R) -> Result<Self>;
}

impl<T> SerializeAsBytes for &T
where
    T: ?Sized + SerializeAsBytes,
{
    fn serialize_ne<W: Write>(&self, write: &mut W) -> Result<()> {
        (**self).serialize_ne(write)
    }

    fn serialize_le<W: Write>(&self, write: &mut W) -> Result<()> {
        (**self).serialize_le(write)
    }

    fn serialize_be<W: Write>(&self, write: &mut W) -> Result<()> {
        (**self).serialize_be(write)
    }
}

impl<T> SerializeAsBytes for &mut T
where
    T: ?Sized + SerializeAsBytes,
{
    fn serialize_ne<W: Write>(&self, write: &mut W) -> Result<()> {
        (**self).serialize_ne(write)
    }

    fn serialize_le<W: Write>(&self, write: &mut W) -> Result<()> {
        (**self).serialize_le(write)
    }

    fn serialize_be<W: Write>(&self, write: &mut W) -> Result<()> {
        (**self).serialize_be(write)
    }
}

impl<T> SerializeAsBytes for Box<T>
where
    T: SerializeAsBytes,
{
    fn serialize_ne<W: Write>(&self, write: &mut W) -> Result<()> {
        (**self).serialize_ne(write)
    }

    fn serialize_le<W: Write>(&self, write: &mut W) -> Result<()> {
        (**self).serialize_le(write)
    }

    fn serialize_be<W: Write>(&self, write: &mut W) -> Result<()> {
        (**self).serialize_be(write)
    }
}

impl<T> DeserializeFromBytes for Box<T>
where
    T: DeserializeFromBytes,
{
    fn deserialize_ne<R: Read>(read: &mut R) -> Result<Self> {
        Ok(Box::new(T::deserialize_ne(read)?))
    }

    fn deserialize_le<R: Read>(read: &mut R) -> Result<Self> {
        Ok(Box::new(T::deserialize_le(read)?))
    }

    fn deserialize_be<R: Read>(read: &mut R) -> Result<Self> {
        Ok(Box::new(T::deserialize_be(read)?))
    }
}

impl<T> SerializeAsBytes for Range<T>
where
    T: SerializeAsBytes,
{
    fn serialize_ne<W: Write>(&self, write: &mut W) -> Result<()> {
        self.start.serialize_ne(write)?;
        self.end.serialize_ne(write)?;
        Ok(())
    }

    fn serialize_le<W: Write>(&self, write: &mut W) -> Result<()> {
        self.start.serialize_le(write)?;
        self.end.serialize_le(write)?;
        Ok(())
    }

    fn serialize_be<W: Write>(&self, write: &mut W) -> Result<()> {
        self.start.serialize_be(write)?;
        self.end.serialize_be(write)?;
        Ok(())
    }
}

impl<T> DeserializeFromBytes for Range<T>
where
    T: DeserializeFromBytes,
{
    fn deserialize_ne<R: Read>(read: &mut R) -> Result<Self> {
        Ok(Self { start: T::deserialize_ne(read)?, end: T::deserialize_ne(read)? })
    }

    fn deserialize_le<R: Read>(read: &mut R) -> Result<Self> {
        Ok(Self { start: T::deserialize_le(read)?, end: T::deserialize_le(read)? })
    }

    fn deserialize_be<R: Read>(read: &mut R) -> Result<Self> {
        Ok(Self { start: T::deserialize_be(read)?, end: T::deserialize_be(read)? })
    }
}

impl<T> SerializeAsBytes for RangeFrom<T>
where
    T: SerializeAsBytes,
{
    fn serialize_ne<W: Write>(&self, write: &mut W) -> Result<()> {
        self.start.serialize_ne(write)
    }

    fn serialize_le<W: Write>(&self, write: &mut W) -> Result<()> {
        self.start.serialize_le(write)
    }

    fn serialize_be<W: Write>(&self, write: &mut W) -> Result<()> {
        self.start.serialize_be(write)
    }
}

impl<T> DeserializeFromBytes for RangeFrom<T>
where
    T: DeserializeFromBytes,
{
    fn deserialize_ne<R: Read>(read: &mut R) -> Result<Self> {
        Ok(Self { start: T::deserialize_ne(read)? })
    }

    fn deserialize_le<R: Read>(read: &mut R) -> Result<Self> {
        Ok(Self { start: T::deserialize_le(read)? })
    }

    fn deserialize_be<R: Read>(read: &mut R) -> Result<Self> {
        Ok(Self { start: T::deserialize_be(read)? })
    }
}

impl SerializeAsBytes for RangeFull {
    fn serialize_ne<W: Write>(&self, _write: &mut W) -> Result<()> {
        Ok(())
    }

    fn serialize_le<W: Write>(&self, _write: &mut W) -> Result<()> {
        Ok(())
    }

    fn serialize_be<W: Write>(&self, _write: &mut W) -> Result<()> {
        Ok(())
    }
}

impl DeserializeFromBytes for RangeFull {
    fn deserialize_ne<R: Read>(_read: &mut R) -> Result<Self> {
        Ok(Self)
    }

    fn deserialize_le<R: Read>(_read: &mut R) -> Result<Self> {
        Ok(Self)
    }

    fn deserialize_be<R: Read>(_read: &mut R) -> Result<Self> {
        Ok(Self)
    }
}

impl<T> SerializeAsBytes for RangeInclusive<T>
where
    T: SerializeAsBytes,
{
    fn serialize_ne<W: Write>(&self, write: &mut W) -> Result<()> {
        self.start().serialize_ne(write)?;
        self.end().serialize_ne(write)?;
        Ok(())
    }

    fn serialize_le<W: Write>(&self, write: &mut W) -> Result<()> {
        self.start().serialize_le(write)?;
        self.end().serialize_le(write)?;
        Ok(())
    }

    fn serialize_be<W: Write>(&self, write: &mut W) -> Result<()> {
        self.start().serialize_be(write)?;
        self.end().serialize_be(write)?;
        Ok(())
    }
}

impl<T> DeserializeFromBytes for RangeInclusive<T>
where
    T: DeserializeFromBytes,
{
    fn deserialize_ne<R: Read>(read: &mut R) -> Result<Self> {
        Ok(Self::new(T::deserialize_ne(read)?, T::deserialize_ne(read)?))
    }

    fn deserialize_le<R: Read>(read: &mut R) -> Result<Self> {
        Ok(Self::new(T::deserialize_le(read)?, T::deserialize_le(read)?))
    }

    fn deserialize_be<R: Read>(read: &mut R) -> Result<Self> {
        Ok(Self::new(T::deserialize_be(read)?, T::deserialize_be(read)?))
    }
}

impl<T> SerializeAsBytes for RangeTo<T>
where
    T: SerializeAsBytes,
{
    fn serialize_ne<W: Write>(&self, write: &mut W) -> Result<()> {
        self.end.serialize_ne(write)
    }

    fn serialize_le<W: Write>(&self, write: &mut W) -> Result<()> {
        self.end.serialize_le(write)
    }

    fn serialize_be<W: Write>(&self, write: &mut W) -> Result<()> {
        self.end.serialize_be(write)
    }
}

impl<T> DeserializeFromBytes for RangeTo<T>
where
    T: DeserializeFromBytes,
{
    fn deserialize_ne<R: Read>(read: &mut R) -> Result<Self> {
        Ok(Self { end: T::deserialize_ne(read)? })
    }

    fn deserialize_le<R: Read>(read: &mut R) -> Result<Self> {
        Ok(Self { end: T::deserialize_le(read)? })
    }

    fn deserialize_be<R: Read>(read: &mut R) -> Result<Self> {
        Ok(Self { end: T::deserialize_be(read)? })
    }
}

impl<T> SerializeAsBytes for RangeToInclusive<T>
where
    T: SerializeAsBytes,
{
    fn serialize_ne<W: Write>(&self, write: &mut W) -> Result<()> {
        self.end.serialize_ne(write)
    }

    fn serialize_le<W: Write>(&self, write: &mut W) -> Result<()> {
        self.end.serialize_le(write)
    }

    fn serialize_be<W: Write>(&self, write: &mut W) -> Result<()> {
        self.end.serialize_be(write)
    }
}

impl<T> DeserializeFromBytes for RangeToInclusive<T>
where
    T: DeserializeFromBytes,
{
    fn deserialize_ne<R: Read>(read: &mut R) -> Result<Self> {
        Ok(Self { end: T::deserialize_ne(read)? })
    }

    fn deserialize_le<R: Read>(read: &mut R) -> Result<Self> {
        Ok(Self { end: T::deserialize_le(read)? })
    }

    fn deserialize_be<R: Read>(read: &mut R) -> Result<Self> {
        Ok(Self { end: T::deserialize_be(read)? })
    }
}

impl<T> SerializeAsBytes for Cow<'_, T>
where
    T: SerializeAsBytes + ToOwned,
{
    fn serialize_ne<W: Write>(&self, write: &mut W) -> Result<()> {
        (**self).serialize_ne(write)
    }

    fn serialize_le<W: Write>(&self, write: &mut W) -> Result<()> {
        (**self).serialize_le(write)
    }

    fn serialize_be<W: Write>(&self, write: &mut W) -> Result<()> {
        (**self).serialize_be(write)
    }
}

impl<T> DeserializeFromBytes for Cow<'_, T>
where
    T: ToOwned,
    <T as ToOwned>::Owned: DeserializeFromBytes,
{
    fn deserialize_ne<R: Read>(read: &mut R) -> Result<Self> {
        Ok(Cow::Owned(<T as ToOwned>::Owned::deserialize_ne(read)?))
    }

    fn deserialize_le<R: Read>(read: &mut R) -> Result<Self> {
        Ok(Cow::Owned(<T as ToOwned>::Owned::deserialize_le(read)?))
    }

    fn deserialize_be<R: Read>(read: &mut R) -> Result<Self> {
        Ok(Cow::Owned(<T as ToOwned>::Owned::deserialize_be(read)?))
    }
}

impl<T> SerializeAsBytes for PhantomData<T> {
    fn serialize_ne<W: Write>(&self, _write: &mut W) -> Result<()> {
        Ok(())
    }

    fn serialize_le<W: Write>(&self, _write: &mut W) -> Result<()> {
        Ok(())
    }

    fn serialize_be<W: Write>(&self, _write: &mut W) -> Result<()> {
        Ok(())
    }
}

impl<T> DeserializeFromBytes for PhantomData<T> {
    fn deserialize_ne<R: Read>(_read: &mut R) -> Result<Self> {
        Ok(Self)
    }

    fn deserialize_le<R: Read>(_read: &mut R) -> Result<Self> {
        Ok(Self)
    }

    fn deserialize_be<R: Read>(_read: &mut R) -> Result<Self> {
        Ok(Self)
    }
}

impl<T> SerializeAsBytes for [T]
where
    T: SerializeAsBytes,
{
    fn serialize_ne<W: Write>(&self, write: &mut W) -> Result<()> {
        for item in self {
            item.serialize_ne(write)?;
        }
        Ok(())
    }

    fn serialize_le<W: Write>(&self, write: &mut W) -> Result<()> {
        for item in self {
            item.serialize_le(write)?;
        }
        Ok(())
    }

    fn serialize_be<W: Write>(&self, write: &mut W) -> Result<()> {
        for item in self {
            item.serialize_be(write)?;
        }
        Ok(())
    }
}

impl<T> SerializeAsBytes for Vec<T>
where
    T: SerializeAsBytes,
{
    fn serialize_ne<W: Write>(&self, write: &mut W) -> Result<()> {
        self.as_slice().serialize_ne(write)
    }

    fn serialize_le<W: Write>(&self, write: &mut W) -> Result<()> {
        self.as_slice().serialize_le(write)
    }

    fn serialize_be<W: Write>(&self, write: &mut W) -> Result<()> {
        self.as_slice().serialize_be(write)
    }
}

impl<T> SerializeAsBytes for VecDeque<T>
where
    T: SerializeAsBytes,
{
    fn serialize_ne<W: Write>(&self, write: &mut W) -> Result<()> {
        for item in self {
            item.serialize_ne(write)?;
        }
        Ok(())
    }

    fn serialize_le<W: Write>(&self, write: &mut W) -> Result<()> {
        for item in self {
            item.serialize_le(write)?;
        }
        Ok(())
    }

    fn serialize_be<W: Write>(&self, write: &mut W) -> Result<()> {
        for item in self {
            item.serialize_be(write)?;
        }
        Ok(())
    }
}

impl<const N: usize, T> SerializeAsBytes for [T; N]
where
    T: SerializeAsBytes,
{
    fn serialize_ne<W: Write>(&self, write: &mut W) -> Result<()> {
        self.as_slice().serialize_ne(write)
    }

    fn serialize_le<W: Write>(&self, write: &mut W) -> Result<()> {
        self.as_slice().serialize_le(write)
    }

    fn serialize_be<W: Write>(&self, write: &mut W) -> Result<()> {
        self.as_slice().serialize_be(write)
    }
}

macro_rules! deserialize_array {
    ($read:expr, $method:ident) => {{
        let mut data: [MaybeUninit<T>; N] = unsafe { MaybeUninit::uninit().assume_init() };
        for i in 0..N {
            let value = match T::$method($read) {
                Ok(value) => value,
                Err(err) => {
                    for elem in &mut data[0..i] {
                        unsafe {
                            elem.assume_init_drop();
                        }
                    }
                    return Err(err);
                }
            };
            data[i].write(value);
        }

        Ok(unsafe { mem::transmute_copy::<[MaybeUninit<T>; N], [T; N]>(&data) })
    }};
}

impl<const N: usize, T> DeserializeFromBytes for [T; N]
where
    T: DeserializeFromBytes,
{
    fn deserialize_ne<R: Read>(read: &mut R) -> Result<Self> {
        deserialize_array!(read, deserialize_ne)
    }

    fn deserialize_le<R: Read>(read: &mut R) -> Result<Self> {
        deserialize_array!(read, deserialize_le)
    }

    fn deserialize_be<R: Read>(read: &mut R) -> Result<Self> {
        deserialize_array!(read, deserialize_be)
    }
}

impl<T> SerializeAsBytes for Wrapping<T>
where
    T: SerializeAsBytes,
{
    fn serialize_ne<W: Write>(&self, write: &mut W) -> Result<()> {
        self.0.serialize_ne(write)
    }

    fn serialize_le<W: Write>(&self, write: &mut W) -> Result<()> {
        self.0.serialize_le(write)
    }

    fn serialize_be<W: Write>(&self, write: &mut W) -> Result<()> {
        self.0.serialize_be(write)
    }
}

impl<T> DeserializeFromBytes for Wrapping<T>
where
    T: DeserializeFromBytes,
{
    fn deserialize_ne<R: Read>(read: &mut R) -> Result<Self> {
        Ok(Wrapping(T::deserialize_ne(read)?))
    }

    fn deserialize_le<R: Read>(read: &mut R) -> Result<Self> {
        Ok(Wrapping(T::deserialize_le(read)?))
    }

    fn deserialize_be<R: Read>(read: &mut R) -> Result<Self> {
        Ok(Wrapping(T::deserialize_be(read)?))
    }
}

impl SerializeAsBytes for bool {
    fn serialize_ne<W: Write>(&self, write: &mut W) -> Result<()> {
        write.write_all(&[*self as u8])?;
        Ok(())
    }

    fn serialize_le<W: Write>(&self, write: &mut W) -> Result<()> {
        self.serialize_ne(write)
    }

    fn serialize_be<W: Write>(&self, write: &mut W) -> Result<()> {
        self.serialize_ne(write)
    }
}

impl DeserializeFromBytes for bool {
    fn deserialize_ne<R: Read>(read: &mut R) -> Result<Self> {
        let mut buf = [0; 1];
        read.read_exact(&mut buf)?;
        Ok(buf[0] != 0)
    }

    fn deserialize_le<R: Read>(read: &mut R) -> Result<Self> {
        Self::deserialize_ne(read)
    }

    fn deserialize_be<R: Read>(read: &mut R) -> Result<Self> {
        Self::deserialize_ne(read)
    }
}

macro_rules! impl_number {
    ($t:ty, $bytes:expr) => {
        impl SerializeAsBytes for $t {
            fn serialize_ne<W: Write>(&self, write: &mut W) -> Result<()> {
                write.write_all(&self.to_ne_bytes())?;
                Ok(())
            }

            fn serialize_le<W: Write>(&self, write: &mut W) -> Result<()> {
                write.write_all(&self.to_le_bytes())?;
                Ok(())
            }

            fn serialize_be<W: Write>(&self, write: &mut W) -> Result<()> {
                write.write_all(&self.to_be_bytes())?;
                Ok(())
            }
        }

        impl DeserializeFromBytes for $t {
            fn deserialize_ne<R: Read>(read: &mut R) -> Result<Self> {
                let mut buf = [0; $bytes];
                read.read_exact(&mut buf)?;
                Ok(Self::from_ne_bytes(buf))
            }

            fn deserialize_le<R: Read>(read: &mut R) -> Result<Self> {
                let mut buf = [0; $bytes];
                read.read_exact(&mut buf)?;
                Ok(Self::from_le_bytes(buf))
            }

            fn deserialize_be<R: Read>(read: &mut R) -> Result<Self> {
                let mut buf = [0; $bytes];
                read.read_exact(&mut buf)?;
                Ok(Self::from_be_bytes(buf))
            }
        }
    };
}

impl_number!(u8, 1);
impl_number!(u16, 2);
impl_number!(u32, 4);
impl_number!(u64, 8);
impl_number!(u128, 16);
impl_number!(usize, std::mem::size_of::<usize>());

impl_number!(i8, 1);
impl_number!(i16, 2);
impl_number!(i32, 4);
impl_number!(i64, 8);
impl_number!(i128, 16);
impl_number!(isize, std::mem::size_of::<isize>());

impl_number!(f32, 4);
impl_number!(f64, 8);

macro_rules! impl_number_non_zero {
    ($t:ty) => {
        impl SerializeAsBytes for $t {
            fn serialize_ne<W: Write>(&self, write: &mut W) -> Result<()> {
                self.get().serialize_ne(write)
            }

            fn serialize_le<W: Write>(&self, write: &mut W) -> Result<()> {
                self.get().serialize_le(write)
            }

            fn serialize_be<W: Write>(&self, write: &mut W) -> Result<()> {
                self.get().serialize_be(write)
            }
        }

        impl DeserializeFromBytes for $t {
            fn deserialize_ne<R: Read>(read: &mut R) -> Result<Self> {
                let inner = DeserializeFromBytes::deserialize_ne(read)?;
                Self::new(inner).ok_or_else(|| ErrorKind::InvalidData.into())
            }

            fn deserialize_le<R: Read>(read: &mut R) -> Result<Self> {
                let inner = DeserializeFromBytes::deserialize_le(read)?;
                Self::new(inner).ok_or_else(|| ErrorKind::InvalidData.into())
            }

            fn deserialize_be<R: Read>(read: &mut R) -> Result<Self> {
                let inner = DeserializeFromBytes::deserialize_be(read)?;
                Self::new(inner).ok_or_else(|| ErrorKind::InvalidData.into())
            }
        }
    };
}

impl_number_non_zero!(NonZeroU8);
impl_number_non_zero!(NonZeroU16);
impl_number_non_zero!(NonZeroU32);
impl_number_non_zero!(NonZeroU64);
impl_number_non_zero!(NonZeroU128);
impl_number_non_zero!(NonZeroUsize);

impl_number_non_zero!(NonZeroI8);
impl_number_non_zero!(NonZeroI16);
impl_number_non_zero!(NonZeroI32);
impl_number_non_zero!(NonZeroI64);
impl_number_non_zero!(NonZeroI128);
impl_number_non_zero!(NonZeroIsize);

macro_rules! impl_atomic {
    ($t:ty) => {
        impl SerializeAsBytes for $t {
            fn serialize_ne<W: Write>(&self, write: &mut W) -> Result<()> {
                self.load(Ordering::Relaxed).serialize_ne(write)
            }

            fn serialize_le<W: Write>(&self, write: &mut W) -> Result<()> {
                self.load(Ordering::Relaxed).serialize_le(write)
            }

            fn serialize_be<W: Write>(&self, write: &mut W) -> Result<()> {
                self.load(Ordering::Relaxed).serialize_be(write)
            }
        }

        impl DeserializeFromBytes for $t {
            fn deserialize_ne<R: Read>(read: &mut R) -> Result<Self> {
                let inner = DeserializeFromBytes::deserialize_ne(read)?;
                Ok(Self::new(inner))
            }

            fn deserialize_le<R: Read>(read: &mut R) -> Result<Self> {
                let inner = DeserializeFromBytes::deserialize_le(read)?;
                Ok(Self::new(inner))
            }

            fn deserialize_be<R: Read>(read: &mut R) -> Result<Self> {
                let inner = DeserializeFromBytes::deserialize_be(read)?;
                Ok(Self::new(inner))
            }
        }
    };
}

impl_atomic!(AtomicBool);

impl_atomic!(AtomicU8);
impl_atomic!(AtomicU16);
impl_atomic!(AtomicU32);
impl_atomic!(AtomicU64);
impl_atomic!(AtomicUsize);

impl_atomic!(AtomicI8);
impl_atomic!(AtomicI16);
impl_atomic!(AtomicI32);
impl_atomic!(AtomicI64);
impl_atomic!(AtomicIsize);

macro_rules! impl_tuple {
    (rec: $field:tt $type:ident, $($rec_field:tt $rec_type:ident),* $(,)?) => {
        impl_tuple!($field $type, $($rec_field $rec_type,)*);
        impl_tuple!(rec: $($rec_field $rec_type,)*);
    };
    (rec:) => {
        impl_tuple!();
    };
    ($($field:tt $type:ident),* $(,)?) => {
        #[allow(unused)] // For empty tuples
        impl<$($type,)*> SerializeAsBytes for ($($type,)*)
        where
            $($type: SerializeAsBytes,)*
        {
            fn serialize_ne<W: Write>(&self, write: &mut W) -> Result<()> {
                let ($($field,)*) = self;
                $($field.serialize_ne(write)?;)*
                Ok(())
            }

            fn serialize_le<W: Write>(&self, write: &mut W) -> Result<()> {
                let ($($field,)*) = self;
                $($field.serialize_le(write)?;)*
                Ok(())
            }

            fn serialize_be<W: Write>(&self, write: &mut W) -> Result<()> {
                let ($($field,)*) = self;
                $($field.serialize_be(write)?;)*
                Ok(())
            }
        }

        #[allow(unused)] // For empty tuples
        impl<$($type,)*> DeserializeFromBytes for ($($type,)*)
        where
            $($type: DeserializeFromBytes,)*
        {
            fn deserialize_ne<R: Read>(read: &mut R) -> Result<Self> {
                Ok(($($type::deserialize_ne(read)?,)*))
            }

            fn deserialize_le<R: Read>(read: &mut R) -> Result<Self> {
                Ok(($($type::deserialize_le(read)?,)*))
            }

            fn deserialize_be<R: Read>(read: &mut R) -> Result<Self> {
                Ok(($($type::deserialize_be(read)?,)*))
            }
        }
    };
}

impl_tuple!(
    rec: a A, b B, c C, d D, e E, f F, g G, h H, i I, j J, k K, l L, m M, n N, o O, p P
);
