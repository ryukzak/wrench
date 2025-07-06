//@ts-ignore
import { test, expect, describe } from 'bun:test'
import { removeComments } from './utils.js'

describe('remove comments properly', () => {
  test('no comments inside a string', () => {
    expect(removeComments('output:         .word 0x84', '\\')).toEqual(
      'output:         .word 0x84',
    )
  })

  test('normal comments preserve whitespace', () => {
    expect(
      removeComments(
        'write_buf_to_memio:             \\ void (const char* from /*a*/, char* to /*b*/)',
        '\\',
      ),
    ).toEqual('write_buf_to_memio:             ')
    expect(removeComments('\\ def hello_user_pstr(input):', '\\')).toEqual('')
  })

  test('handle string escaping', () => {
    {
      const str = "prompt:         .byte 19, 'What is your name?\\n'"
      expect(removeComments(str, '\\')).toEqual(str)
    }
    {
      const str = 'prompt:         .byte 19, "What is your name?\\n"'
      expect(removeComments(str, '\\')).toEqual(str)
    }
    {
      const str =
        'prompt:         .byte 19, "What is your name?\\n"  \\ skibidi comment'
      const output_str = 'prompt:         .byte 19, "What is your name?\\n"  '

      expect(removeComments(str, '\\')).toEqual(output_str)
    }
  })

  test('handles escaping quotes', () => {
    expect(removeComments('"\\""\\comment', '\\')).toBe('"\\""')
    expect(removeComments('"\\"\\n"', '\\')).toBe('"\\"\\n"')
  })

  test('removes comment outside string', () => {
    expect(removeComments('abc;def', ';')).toBe('abc')
  })

  test('preserves content when comment inside double quotes', () => {
    expect(removeComments('abc";def', ';')).toBe('abc";def')
  })

  test('preserves content when comment inside single quotes', () => {
    expect(removeComments("abc';def", ';')).toBe("abc';def")
  })

  test('processes escaped double quote inside string', () => {
    expect(removeComments('a\\"b', ';')).toBe('a\\"b')
  })

  test('processes escaped single quote inside string', () => {
    expect(removeComments("a\\'b", ';')).toBe("a\\'b")
  })

  test('closes double quoted string properly after escaped quote', () => {
    expect(removeComments('abc"def\\"ghi"jkl', ';')).toBe('abc"def\\"ghi"jkl')
  })

  test('closes single quoted string properly after escaped quote', () => {
    expect(removeComments("abc'def\\'ghi'jkl", ';')).toBe("abc'def\\'ghi'jkl")
  })

  test('comment after closed string', () => {
    expect(removeComments('abc"def";ghi', ';')).toBe('abc"def"')
  })

  test('handles escaped comment starter outside string', () => {
    expect(removeComments('abc\\;def', ';')).toBe('abc\\')
  })

  test('handles multiple escape sequences', () => {
    expect(removeComments('echo("Hello \\\\n");;// comment', '\\')).toBe(
      'echo("Hello \\\\n");;// comment',
    )
  })

  test('unterminated string with comment inside', () => {
    expect(removeComments('var s = "abc;def', ';')).toBe('var s = "abc;def')
  })

  test('mix of quotes and comment starters inside strings', () => {
    expect(removeComments('a="b;\'c"; // comment', ';')).toBe('a="b;\'c"')
  })

  test('comment starter immediately after closing quote', () => {
    expect(removeComments('"abc";def', ';')).toBe('"abc"')
  })

  test('handles both single and double quotes in code but not strings', () => {
    expect(removeComments("a='x'; b='y';", ';')).toBe("a='x'")
  })

  test('escaped comment starter within single quoted string', () => {
    expect(removeComments("a='\\;';b", ';')).toBe("a='\\;'")
  })
})
