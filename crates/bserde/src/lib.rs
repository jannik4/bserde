#![deny(rust_2018_idioms)]

use std::{
    io::{Read, Result, Write},
    marker::PhantomData,
    mem::{self, MaybeUninit},
};

#[cfg(feature = "derive")]
pub use bserde_derive::{DeserializeFromBytes, SerializeAsBytes};

pub trait SerializeAsBytes {
    fn serialize_ne<W: Write>(&self, write: W) -> Result<()>;
    fn serialize_le<W: Write>(&self, write: W) -> Result<()>;
    fn serialize_be<W: Write>(&self, write: W) -> Result<()>;

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
    fn deserialize_ne<R: Read>(read: R) -> Result<Self>;
    fn deserialize_le<R: Read>(read: R) -> Result<Self>;
    fn deserialize_be<R: Read>(read: R) -> Result<Self>;
}

impl<T> SerializeAsBytes for &T
where
    T: ?Sized + SerializeAsBytes,
{
    fn serialize_ne<W: Write>(&self, write: W) -> Result<()> {
        (**self).serialize_ne(write)
    }

    fn serialize_le<W: Write>(&self, write: W) -> Result<()> {
        (**self).serialize_le(write)
    }

    fn serialize_be<W: Write>(&self, write: W) -> Result<()> {
        (**self).serialize_be(write)
    }
}

impl<T> SerializeAsBytes for &mut T
where
    T: ?Sized + SerializeAsBytes,
{
    fn serialize_ne<W: Write>(&self, write: W) -> Result<()> {
        (**self).serialize_ne(write)
    }

    fn serialize_le<W: Write>(&self, write: W) -> Result<()> {
        (**self).serialize_le(write)
    }

    fn serialize_be<W: Write>(&self, write: W) -> Result<()> {
        (**self).serialize_be(write)
    }
}

impl<T> SerializeAsBytes for Box<T>
where
    T: SerializeAsBytes,
{
    fn serialize_ne<W: Write>(&self, write: W) -> Result<()> {
        (**self).serialize_ne(write)
    }

    fn serialize_le<W: Write>(&self, write: W) -> Result<()> {
        (**self).serialize_le(write)
    }

    fn serialize_be<W: Write>(&self, write: W) -> Result<()> {
        (**self).serialize_be(write)
    }
}

impl<T> DeserializeFromBytes for Box<T>
where
    T: DeserializeFromBytes,
{
    fn deserialize_ne<R: Read>(read: R) -> Result<Self> {
        Ok(Box::new(T::deserialize_ne(read)?))
    }

    fn deserialize_le<R: Read>(read: R) -> Result<Self> {
        Ok(Box::new(T::deserialize_le(read)?))
    }

    fn deserialize_be<R: Read>(read: R) -> Result<Self> {
        Ok(Box::new(T::deserialize_be(read)?))
    }
}

impl<T> SerializeAsBytes for PhantomData<T> {
    fn serialize_ne<W: Write>(&self, _write: W) -> Result<()> {
        Ok(())
    }

    fn serialize_le<W: Write>(&self, _write: W) -> Result<()> {
        Ok(())
    }

    fn serialize_be<W: Write>(&self, _write: W) -> Result<()> {
        Ok(())
    }
}

impl<T> DeserializeFromBytes for PhantomData<T> {
    fn deserialize_ne<R: Read>(_read: R) -> Result<Self> {
        Ok(Self)
    }

    fn deserialize_le<R: Read>(_read: R) -> Result<Self> {
        Ok(Self)
    }

    fn deserialize_be<R: Read>(_read: R) -> Result<Self> {
        Ok(Self)
    }
}

impl<T> SerializeAsBytes for [T]
where
    T: SerializeAsBytes,
{
    fn serialize_ne<W: Write>(&self, mut write: W) -> Result<()> {
        for item in self {
            item.serialize_ne(&mut write)?;
        }
        Ok(())
    }

    fn serialize_le<W: Write>(&self, mut write: W) -> Result<()> {
        for item in self {
            item.serialize_le(&mut write)?;
        }
        Ok(())
    }

    fn serialize_be<W: Write>(&self, mut write: W) -> Result<()> {
        for item in self {
            item.serialize_be(&mut write)?;
        }
        Ok(())
    }
}

impl<T> SerializeAsBytes for Vec<T>
where
    T: SerializeAsBytes,
{
    fn serialize_ne<W: Write>(&self, write: W) -> Result<()> {
        self.as_slice().serialize_ne(write)
    }

    fn serialize_le<W: Write>(&self, write: W) -> Result<()> {
        self.as_slice().serialize_le(write)
    }

    fn serialize_be<W: Write>(&self, write: W) -> Result<()> {
        self.as_slice().serialize_be(write)
    }
}

impl<const N: usize, T> SerializeAsBytes for [T; N]
where
    T: SerializeAsBytes,
{
    fn serialize_ne<W: Write>(&self, write: W) -> Result<()> {
        self.as_slice().serialize_ne(write)
    }

    fn serialize_le<W: Write>(&self, write: W) -> Result<()> {
        self.as_slice().serialize_le(write)
    }

    fn serialize_be<W: Write>(&self, write: W) -> Result<()> {
        self.as_slice().serialize_be(write)
    }
}

macro_rules! deserialize_array {
    ($read:expr, $method:ident) => {{
        let mut data: [MaybeUninit<T>; N] = unsafe { MaybeUninit::uninit().assume_init() };
        for i in 0..N {
            let value = match T::$method(&mut $read) {
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
    fn deserialize_ne<R: Read>(mut read: R) -> Result<Self> {
        deserialize_array!(read, deserialize_ne)
    }

    fn deserialize_le<R: Read>(mut read: R) -> Result<Self> {
        deserialize_array!(read, deserialize_le)
    }

    fn deserialize_be<R: Read>(mut read: R) -> Result<Self> {
        deserialize_array!(read, deserialize_be)
    }
}

impl SerializeAsBytes for bool {
    fn serialize_ne<W: Write>(&self, mut write: W) -> Result<()> {
        write.write_all(&[*self as u8])?;
        Ok(())
    }

    fn serialize_le<W: Write>(&self, write: W) -> Result<()> {
        self.serialize_ne(write)
    }

    fn serialize_be<W: Write>(&self, write: W) -> Result<()> {
        self.serialize_ne(write)
    }
}

impl DeserializeFromBytes for bool {
    fn deserialize_ne<R: Read>(mut read: R) -> Result<Self> {
        let mut buf = [0; 1];
        read.read_exact(&mut buf)?;
        Ok(buf[0] != 0)
    }

    fn deserialize_le<R: Read>(read: R) -> Result<Self> {
        Self::deserialize_ne(read)
    }

    fn deserialize_be<R: Read>(read: R) -> Result<Self> {
        Self::deserialize_ne(read)
    }
}

macro_rules! impl_number {
    ($t:ty, $bytes:expr) => {
        impl SerializeAsBytes for $t {
            fn serialize_ne<W: Write>(&self, mut write: W) -> Result<()> {
                write.write_all(&self.to_ne_bytes())?;
                Ok(())
            }

            fn serialize_le<W: Write>(&self, mut write: W) -> Result<()> {
                write.write_all(&self.to_le_bytes())?;
                Ok(())
            }

            fn serialize_be<W: Write>(&self, mut write: W) -> Result<()> {
                write.write_all(&self.to_be_bytes())?;
                Ok(())
            }
        }

        impl DeserializeFromBytes for $t {
            fn deserialize_ne<R: Read>(mut read: R) -> Result<Self> {
                let mut buf = [0; $bytes];
                read.read_exact(&mut buf)?;
                Ok(Self::from_ne_bytes(buf))
            }

            fn deserialize_le<R: Read>(mut read: R) -> Result<Self> {
                let mut buf = [0; $bytes];
                read.read_exact(&mut buf)?;
                Ok(Self::from_le_bytes(buf))
            }

            fn deserialize_be<R: Read>(mut read: R) -> Result<Self> {
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
            fn serialize_ne<W: Write>(&self, mut write: W) -> Result<()> {
                let ($($field,)*) = self;
                $($field.serialize_ne(&mut write)?;)*
                Ok(())
            }

            fn serialize_le<W: Write>(&self, mut write: W) -> Result<()> {
                let ($($field,)*) = self;
                $($field.serialize_le(&mut write)?;)*
                Ok(())
            }

            fn serialize_be<W: Write>(&self, mut write: W) -> Result<()> {
                let ($($field,)*) = self;
                $($field.serialize_be(&mut write)?;)*
                Ok(())
            }
        }

        #[allow(unused)] // For empty tuples
        impl<$($type,)*> DeserializeFromBytes for ($($type,)*)
        where
            $($type: DeserializeFromBytes,)*
        {
            fn deserialize_ne<R: Read>(mut read: R) -> Result<Self> {
                Ok(($($type::deserialize_ne(&mut read)?,)*))
            }

            fn deserialize_le<R: Read>(mut read: R) -> Result<Self> {
                Ok(($($type::deserialize_le(&mut read)?,)*))
            }

            fn deserialize_be<R: Read>(mut read: R) -> Result<Self> {
                Ok(($($type::deserialize_be(&mut read)?,)*))
            }
        }
    };
}

impl_tuple!(
    rec: a A, b B, c C, d D, e E, f F, g G, h H, i I, j J, k K, l L, m M, n N, o O, p P
);
