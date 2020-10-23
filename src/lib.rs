// An experiment to better understand nom's implementation and ideas

#[derive(Debug)]
pub enum ErrorKind {
    ParseError,
    EncodingError,
    Take,
    TakeUntil,
    Integer,
    Verify,
    Byte,
    Tag,
    Eof,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Endianness {
    Big,
    Little,
}

#[derive(Debug)]
pub struct Error<'a> {
    pub input: &'a[u8],
    pub code: ErrorKind,
}

impl<'a> Error<'a> {
    fn new(i: &'a[u8], ek: ErrorKind) -> Self {
        Error { input: i, code: ek }
    }
}

pub type IResult<'a, O> = Result<(&'a[u8], O), Error<'a>>;

// readers
pub fn take<'a>(n: usize) -> impl Fn(&'a[u8]) -> IResult<&'a[u8]> {
    move |i:&[u8]| {
        if i.len() == n {
            Ok((&[], i))
        } else if i.len() > n {
            let (a, b) = i.split_at(n);
            Ok((b, a))
        } else {
            Err(Error::new(i, ErrorKind::Take))
        }
    }
}

pub fn take_until<'a>(pat: &'a[u8]) -> impl Fn(&'a[u8]) -> IResult<&'a[u8]> {
    move |i:&[u8]| {
        let p = i.windows(pat.len()).position(|x| x == pat);
        if let Some(p) = p {
            let (a, b) = i.split_at(p);
            Ok((b, a))
        } else {
            Err(Error::new(i, ErrorKind::TakeUntil))
        }
    }
}

pub fn u64<'a>(e: Endianness) -> impl Fn(&'a[u8]) -> IResult<u64> {
    move |i:&[u8]| {
        if let Ok((r, i)) = take(8)(i) {
            let (i, u1) = u32(e)(i)?;
            let (i, u2) = u32(e)(i)?;
            Ok((r, (u1 as u64) << 32 | u2 as u64))
        } else {
            Err(Error::new(i, ErrorKind::Integer))
        }
    }
}

pub fn be_u64<'a>(i: &'a[u8]) -> IResult<u64> { u64(Endianness::Big)(i) }
pub fn le_u64<'a>(i: &'a[u8]) -> IResult<u64> { u64(Endianness::Little)(i) }

pub fn u32<'a>(e: Endianness) -> impl Fn(&'a[u8]) -> IResult<u32> {
    move |i:&[u8]| {
        if let Ok((a, b)) = take(4)(i) {
                let u = if e == Endianness::Big {
                (b[0] as u32) << 24 | (b[1] as u32) << 16 | (b[2] as u32) << 8 | b[3] as u32
            } else {                                                     
                (b[3] as u32) << 24 | (b[2] as u32) << 16 | (b[1] as u32) << 8 | b[0] as u32
            };
            Ok((a, u))
        } else {
            Err(Error::new(i, ErrorKind::Integer))
        }
    }
}

pub fn be_u32<'a>(i: &'a[u8]) -> IResult<u32> { u32(Endianness::Big)(i) }
pub fn le_u32<'a>(i: &'a[u8]) -> IResult<u32> { u32(Endianness::Little)(i) }

pub fn u16<'a>(e: Endianness) -> impl Fn(&'a[u8]) -> IResult<u16> {
    move |i:&[u8]| {
        if let Ok((i, b)) = take(2)(i) {
            let u = if e == Endianness::Big {
                (b[0] as u16) << 8 | b[1] as u16
            } else {                                                     
                (b[1] as u16) << 8 | b[0] as u16
            };
            Ok((i, u))
        } else {
            Err(Error::new(i, ErrorKind::Integer))
        }
    }
}

pub fn be_u16<'a>(i: &'a[u8]) -> IResult<u16> { u16(Endianness::Big)(i) }
pub fn le_u16<'a>(i: &'a[u8]) -> IResult<u16> { u16(Endianness::Little)(i) }


pub fn u8<'a>(i:&'a[u8]) -> IResult<u8> {
    if !i.is_empty() {
        Ok((&i[1..], i[0]))
    } else {
        Err(Error::new(i, ErrorKind::Byte))
    }
}

pub fn be_u8<'a>(i:&'a[u8]) -> IResult<u8> {
    u8(i)
}

// matchers
pub fn byte<'a>(b: u8) -> impl Fn(&'a[u8]) -> IResult<u8> {
    move |i:&[u8]| {
        if !i.is_empty() && i[0] == b {
            Ok((&i[1..], b))
        } else {
            Err(Error::new(i, ErrorKind::Byte))
        }
    }
}

pub fn tag<'a>(t: &'a[u8]) -> impl Fn(&'a[u8]) -> IResult<&'a[u8]> {
    move |i:&[u8]| {
        if i.len() >= t.len() {
            let (a, b) = i.split_at(t.len());
            if a == t {
                return Ok((b, a));
            }
        }
        Err(Error::new(i, ErrorKind::Tag))
    }
}

// Combinators
pub fn many0<'a, O>(p: impl Fn(&'a[u8]) -> IResult<O>) -> impl Fn(&'a[u8]) -> IResult<Vec<O>> {
    move |mut i:&[u8]| {
        let mut r = Vec::new();
        while let Ok((xi, x)) = p(i) {
            r.push(x);
            i = xi;
        }
        Ok((i, r))
    }
}

pub fn many1<'a, O>(p: impl Fn(&'a[u8]) -> IResult<O>) -> impl Fn(&'a[u8]) -> IResult<Vec<O>> {
    move |i:&[u8]| {
        let mut r = Vec::with_capacity(1);
        let (mut i, x) = p(i)?;  // need at least one
        r.push(x);
        while let Ok((xi, x)) = p(i) {
            r.push(x);
            i = xi;
        }
        Ok((i, r))
    }
}

pub fn alt2<'a, O>(a: impl Fn(&'a[u8]) -> IResult<O>, b: impl Fn(&'a[u8]) -> IResult<O>) -> impl Fn(&'a[u8]) -> IResult<O> {
    move |i:&[u8]| {
        match a(i) {
            Ok(x) => Ok(x),
            _ => b(i),   // could compose an error... chose not to.
        }
    }
}

pub fn verify<'a, O>(p: impl Fn(&'a[u8]) -> IResult<O>, f: impl Fn(&O) -> bool) -> impl Fn(&'a[u8])-> IResult<O> {
    move |i:&[u8]| {
        let (pi, r) = p(i)?;
        if f(&r) {
            Ok((pi, r))
        } else {
            Err(Error::new(i, ErrorKind::Verify))
        }
    }
}

pub fn map<'a, O1, O2>(p: impl Fn(&'a[u8]) -> IResult<O1>, f: impl Fn(O1) -> O2) -> impl Fn(&'a[u8]) -> IResult<O2> {
    move |i:&[u8]| {
        let (pi, r) = p(i)?;
        Ok((pi, f(r)))
    }
}

pub fn many_till<'a, O1, O2>(first: impl Fn(&'a[u8]) -> IResult<O1>, second: impl Fn(&'a[u8]) -> IResult<O2>) -> impl Fn(&'a[u8]) -> IResult<(Vec<O1>, O2)> {
    move |mut i:&[u8]| {
        let mut v = Vec::new();
        loop {
            if let Ok((i, y)) = second(i) {
                return Ok((i, (v, y)));
            }
            let (xi, x) = first(i)?;
            v.push(x);
            i = xi;
        }
        // unreachable!();
    }
}

pub fn eof(i:&[u8]) -> IResult<()> {
    if i.is_empty() {
        Ok((i, ()))
    } else {
        Err(Error::new(i, ErrorKind::Eof))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_take() {
        let data = b"1234567";
        let r = take(3)(data);
        assert!(r.is_ok());
        let r = r.unwrap();
        assert_eq!(r.0, b"4567");
        assert_eq!(r.1, b"123");
    }

    #[test]
    fn test_take_until() {
        let data = b"123\0456";
        let r = take_until(b"\0")(data);
        assert!(r.is_ok());
        let r = r.unwrap();
        assert_eq!(r.0, b"\0456");
        assert_eq!(r.1, b"123");
    }

    #[test]
    fn test_many0() {
        let data = b"123456789ab";
        let r = many0(take(3))(data);
        assert!(r.is_ok());
        let r = r.unwrap();
        assert_eq!(r.0, b"ab");
        assert_eq!(r.1, vec![b"123", b"456", b"789"]);
        let data = b"ab";
        let r = many0(take(3))(data);
        assert!(r.is_ok());
        let r = r.unwrap();
        assert_eq!(r.0, b"ab");
        assert!(r.1.is_empty());
    }

    #[test]
    fn test_many1() {
        let data = b"ab";
        let r = many1(take(3))(data);
        assert!(r.is_err());
        let data = b"123ab";
        let r = many1(take(3))(data);
        assert!(r.is_ok());
        let r = r.unwrap();
        assert_eq!(r.0, b"ab");
        assert_eq!(r.1.len(), 1);
        assert_eq!(r.1, vec![b"123"]);
    }

    #[test]
    fn test_u32() {
        let data = b"\x11\x22\x33\x44";
        let r = u32(Endianness::Little)(data);
        assert!(r.is_ok());
        let r = r.unwrap();
    }

    #[test]
    fn test_u16() {
        let data = b"\x11\x22\x33\x44";
        let r = u16(Endianness::Little)(data);
        assert!(r.is_ok());
        let r = r.unwrap();
    }

    #[test]
    fn test_verify() {
        let data = b"ab";
        let r = verify(take(1), |x| x == b"a")(data);
        assert!(r.is_ok());
        let r = r.unwrap();
        assert_eq!(r.0, b"b");
        assert_eq!(r.1, b"a");

        let data = b"ab";
        let r = verify(take(1), |x| x == b"b")(data);
        assert!(r.is_err());
    }

    #[test]
    fn test_byte() {
        let data = b"\0ab";
        let r = byte(0)(data);
        assert!(r.is_ok());
        let r = r.unwrap();
        assert_eq!(r.0, b"ab");
        assert_eq!(r.1, 0);
    }

    #[test]
    fn test_map() {
        let data = b"\0ab";
        let r = map(byte(0), |x| x+1)(data);
        assert!(r.is_ok());
        let r = r.unwrap();
        assert_eq!(r.0, b"ab");
        assert_eq!(r.1, 1);
    }

    #[test]
    fn test_tag() {
        let data = b"123abcde";
        let r = tag(b"123")(data);
        assert!(r.is_ok());
        let r = r.unwrap();
        assert_eq!(r.0, b"abcde");
        assert_eq!(r.1, b"123");
        let r = tag(b"abc")(data);
        assert!(r.is_err());
    }

    #[test]
    fn test_many_till() {
        let data = b"121212abc";
        let r = many_till(tag(b"12"), byte(b'a'))(data);
        assert!(r.is_ok());
        let r = r.unwrap();
        assert_eq!(r.0, b"bc");
        assert_eq!(r.1.0, vec![b"12", b"12", b"12"]);
        assert_eq!(r.1.1, b'a');
    }

    #[test]
    fn test_take_at_end() {
        let data = b"123";
        let r = take(3)(data);
        assert!(r.is_ok());
        let r = r.unwrap();
        assert_eq!(r.0, b"");
        assert_eq!(r.1, b"123");
    }

    #[test]
    fn test_many_till_eof() {
        let data = b"121212ab";
        let r = many_till(alt2(tag(b"12"), tag(b"ab")), eof)(data);
        assert!(r.is_ok());
        let r = r.unwrap();
        assert_eq!(r.0, b"");
        assert_eq!(r.1.0, vec![b"12", b"12", b"12", b"ab"]);
        assert_eq!(r.1.1, ());

        let data = b"121212ab1";
        let r = many_till(alt2(tag(b"12"), tag(b"ab")), eof)(data);
        assert!(r.is_err());

        let data = b"121212ab";
        let r = many_till(take(2), eof)(data);
        assert!(r.is_ok());
        let r = r.unwrap();
        assert_eq!(r.0, b"");
        assert_eq!(r.1.0, vec![b"12", b"12", b"12", b"ab"]);
        assert_eq!(r.1.1, ());
    }
}
