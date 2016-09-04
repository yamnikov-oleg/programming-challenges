use std::iter::Cycle;
use std::mem::swap;

pub mod prelude {
    pub use super::{WeaveInto, WovenWith};
}

#[derive(Clone, Copy)]
enum WeaveTurn {
    First,
    Second,
}

pub struct WeaveIter<I1, I2, It>
    where I1: Iterator<Item = It>,
          I2: Iterator<Item = It>
{
    first: I1,
    second: I2,
    turn: WeaveTurn,
    sec_ahead: Option<It>,
}

impl<I1, I2, It> WeaveIter<Cycle<I1>, I2, It>
    where I1: Iterator<Item = It> + Clone,
          I2: Iterator<Item = It>
{
    pub fn new<IntoI1, IntoI2>(a: IntoI1, b: IntoI2) -> WeaveIter<Cycle<I1>, I2, It>
        where IntoI1: IntoIterator<IntoIter = I1, Item = I1::Item>,
              IntoI2: IntoIterator<IntoIter = I2, Item = I2::Item>
    {
        WeaveIter {
            first: a.into_iter().cycle(),
            second: b.into_iter(),
            turn: WeaveTurn::Second,
            sec_ahead: None,
        }
    }
}

impl<I1, I2, It> Iterator for WeaveIter<I1, I2, It>
    where I1: Iterator<Item = It>,
          I2: Iterator<Item = It>
{
    type Item = It;

    fn next(&mut self) -> Option<It> {
        match (self.turn, &mut self.sec_ahead) {
            (WeaveTurn::First, &mut Some(_)) => {
                self.turn = WeaveTurn::Second;
                self.first.next()
            }
            (WeaveTurn::First, &mut None) => None,
            (WeaveTurn::Second, ahead) => {
                self.turn = WeaveTurn::First;
                let mut cur = self.second.next();
                match ahead {
                    &mut None => *ahead = self.second.next(),
                    &mut Some(_) => swap(&mut cur, ahead),
                };
                cur
            }
        }
    }
}

pub trait WeaveInto<I>: IntoIterator + Sized
    where I: IntoIterator<Item = Self::Item>,
          Self::IntoIter: Sized + Clone
{
    fn weave_into(self, other: I) -> WeaveIter<Cycle<Self::IntoIter>, I::IntoIter, I::Item> {
        WeaveIter::new(self, other)
    }
}

impl<I, T> WeaveInto<I> for T
    where T: IntoIterator + Sized,
          I: IntoIterator<Item = T::Item>,
          T::IntoIter: Sized + Clone
{
}

pub trait WovenWith<I>: IntoIterator + Sized
    where I: IntoIterator<Item = Self::Item>,
          I::IntoIter: Clone
{
    fn woven_with(self, other: I) -> WeaveIter<Cycle<I::IntoIter>, Self::IntoIter, I::Item> {
        WeaveIter::new(other, self)
    }
}

impl<I, T> WovenWith<I> for T
    where I: IntoIterator + Sized,
          T: IntoIterator<Item = I::Item>,
          I::IntoIter: Sized + Clone
{
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn weave_iter1() {
        let a = vec![1, 2, 3, 4];
        let b = vec![-1, -2];

        let r: Vec<i32> = WeaveIter::new(a, b).collect();
        assert_eq!(r, &[-1, 1, -2]);
    }

    #[test]
    fn weave_iter2() {
        let a = vec![-1, -2];
        let b = vec![1, 2, 3, 4];

        let r: Vec<i32> = WeaveIter::new(&a, &b).cloned().collect();
        assert_eq!(r, &[1, -1, 2, -2, 3, -1, 4]);
    }

    #[test]
    fn weave_into() {
        let a = vec!['a', 'b', 'c'];
        let b = vec!['1', '2', '3', '4', '5', '6'];

        let r: Vec<char> = a.weave_into(b.into_iter()).collect();
        assert_eq!(r, &['1', 'a', '2', 'b', '3', 'c', '4', 'a', '5', 'b', '6']);
    }

    #[test]
    fn woven_with() {
        let a = "space";
        let b = " ";

        let r: String = a.chars().woven_with(b.chars()).collect();
        assert_eq!(r, "s p a c e");
    }
}
