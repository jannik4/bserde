use bserde::{DeserializeFromBytes, SerializeAsBytes};

#[derive(Debug, SerializeAsBytes, DeserializeFromBytes)]
struct TestA {
    a: u8,
    b: u16,
    c: u32,
    d: u64,
}

#[derive(Debug, PartialEq, Eq, SerializeAsBytes, DeserializeFromBytes)]
#[trailing_padding(0xA)]
struct TestB<T, U> {
    a: u8,
    #[padding(1)]
    b: u16,
    #[padding(4)]
    d: u64,
    c: u32,
    data: T,
    _marker: std::marker::PhantomData<U>,
    _xxx: Xxx<U>,
}

#[derive(Debug, PartialEq, Eq, SerializeAsBytes, DeserializeFromBytes)]
struct Xxx<T> {
    _marker: std::marker::PhantomData<T>,
}

#[derive(Debug, SerializeAsBytes, DeserializeFromBytes)]
struct TestUnit;

#[derive(Debug, SerializeAsBytes, DeserializeFromBytes)]
struct TestTupleStruct(u8, #[padding(4)] u32, bool);

#[allow(unused)]
fn assert_implementations() {
    assert_has_impl::<TestA>();
    assert_has_impl::<TestB<u8, String>>();
    assert_has_impl::<TestUnit>();
    assert_has_impl::<TestTupleStruct>();
}

fn assert_has_impl<T: DeserializeFromBytes + SerializeAsBytes>() {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let test_b = TestB {
            a: 0x01,
            b: 0x0203,
            c: 0x04050607,
            d: 0x08090A0B0C0D0E0F,
            data: true,
            _marker: std::marker::PhantomData::<String>,
            _xxx: Xxx { _marker: std::marker::PhantomData },
        };

        assert_eq!(
            test_b,
            TestB::deserialize_ne(&mut test_b.serialize_ne_to_vec().as_slice()).unwrap()
        );
    }
}
