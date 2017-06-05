package com.msosnicki.fixed

import org.scalacheck.Gen
import org.scalatest.prop.{Configuration, PropertyChecks}
import org.scalatest.{DiagrammedAssertions, FlatSpec, FunSuite, Matchers}
import scodec.bits.BitVector

class FixedTest extends FlatSpec with Matchers with PropertyChecks with Configuration with DiagrammedAssertions {

  behavior of "Char UTF8 codec"

  implicit val generatorDrivenConf =
    PropertyCheckConfiguration(minSuccessful = 200)

  it should "get to the same result after encoding -> decoding actions" in {
    forAll { char: Char =>
      val bytes = Fixed.charUtf8.encode(char).require
      val decoded = Fixed.charUtf8.decode(bytes).require
      assert(decoded.value === char)
      assert(decoded.remainder.size === 0)
    }
  }

  behavior of "string codec derived from char UTF8 codec"

  val utf8FromChar = scodec.codecs.list(Fixed.charUtf8).xmap[String](_.mkString, _.toList)

  it should "get the same result after encoding -> decoding actions" in {
    forAll { string: String =>
      val bytes = utf8FromChar.encode(string).require
      val decoded = utf8FromChar.decode(bytes).require
      assert(decoded.value === string)
      assert(decoded.remainder.size === 0)
    }
  }

  behavior of "fixed lenght codec"

  it should "encode all the values correctly" in {
    forAll(Gen.chooseNum(5, 100), Gen.alphaNumChar, Gen.alphaNumStr) { (size: Int, filler: Char, string: String) =>
      val codec = Fixed.fixed(size, filler)
      val encoded = codec.encode(string)
      if (string.length > size) {
        assert(encoded.isFailure)
      }
      else if (string.length == size) {
        assert(encoded.require.utf8 === string)
      }
      else {
        val padding = size - string.length
        assert(encoded.require.utf8 === List.fill(padding)(filler).mkString ++ string)
      }
    }
  }

  it should "decode all the values correctly" in {
    forAll(Gen.chooseNum(10, 100), Gen.alphaNumChar, Gen.alphaNumStr) { (size: Int, filler: Char, string: String) =>
      val codec = Fixed.fixed(size, filler)
      val encoded = codec.encode(string)
      if (encoded.isSuccessful) {
        val fixed = encoded.require
        val decoded = codec.decode(fixed).require
        assert(decoded.value === string.dropWhile(_ == filler)) //TODO: there is no way to recognize the beginning if it starts with filler, so it has to be dropped?
        assert(decoded.remainder.isEmpty)
      }
    }
  }

  implicit class StringOp(string: String) {
    lazy val utf8 = string.getBytes("UTF-8")
  }

  implicit class BytesOps(bytes: BitVector) {
    lazy val utf8 = new String(bytes.toByteArray, "UTF-8")
  }

}
