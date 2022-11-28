package main

import (
	"bufio"
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
)

var codes = map[string]int{
	"富强": 0,
	"民主": 1,

	"文明": 2,
	"和谐": 3,

	"自由": 4,
	"平等": 5,

	"公正": 6,
	"法治": 7,

	"爱国": 8,
	"敬业": 9,

	"诚信": 10,
	"友善": 11,
}

var encodeCodeMap = map[byte][]byte{
	0: []byte("富强"),
	1: []byte("民主"),

	2: []byte("文明"),
	3: []byte("和谐"),

	4: []byte("自由"),
	5: []byte("平等"),

	6: []byte("公正"),
	7: []byte("法治"),

	8: []byte("爱国"),
	9: []byte("敬业"),

	10: []byte("诚信富强"),
	11: []byte("友善文明"),
	12: []byte("友善平等"),
	13: []byte("友善法治"),
	14: []byte("诚信爱国"),
	15: []byte("诚信友善"),
}

var decodeCodeMap = map[string]byte{}

func init() {
	for k, v := range encodeCodeMap {
		decodeCodeMap[string(v)] = k
	}
}

var (
	in   = flag.String("i", "", "input file path")
	out  = flag.String("o", "", "output file path")
	mode = flag.String("m", "en", "mode en(encode)/de(decode)")
)

func main() {
	flag.Parse()
	reader, writer, err, close := inout()
	defer close()
	if err != nil {
		fmt.Fprintf(os.Stderr, "%s", err)
		return
	}

	if *mode == "en" {
		err = encode(reader, writer)
	} else if *mode == "de" {
		err = decode(reader, writer)
	} else {
		err = fmt.Errorf("unknow mode")
	}

	if err != nil {
		fmt.Fprintf(os.Stderr, "%s", err)
	}
}

// 实现感觉很别扭, 但没想到什么好的方法
func inout() (reader io.Reader, writer io.Writer, e error, close func()) {
	var inCloser io.Closer
	var outCloser io.Closer

	close = func() {
		if inCloser != nil {
			inCloser.Close()
		}
		if outCloser != nil {
			outCloser.Close()
		}
	}

	if *in != "" {
		f, err := os.Open(*in)
		if err != nil {
			e = err
			return
		}
		reader = f
		inCloser = f
	} else {
		fileInfo, _ := os.Stdin.Stat()
		if (fileInfo.Mode() & os.ModeNamedPipe) != os.ModeNamedPipe {
			e = errors.New("input is file or pipeline")
			return
		}
		reader = os.Stdin
	}

	if *out != "" {
		f, err := os.Open(*out)
		if os.IsNotExist(err) {
			f, err = os.OpenFile(*out, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0644)
			if err != nil {
				e = err
				return
			}
		}
		fileInfo, err := f.Stat()
		if err != nil {
			e = err
			return
		}
		if fileInfo.IsDir() {
			e = errors.New("outfile is a dir")
			return
		}
		writer = f
		outCloser = f
	} else {
		writer = os.Stdout
	}
	return
}

func encode(r io.Reader, wr io.Writer) error {
	bs := make([]byte, 1024)
	for {
		size, err := r.Read(bs)
		if err != nil {
			return nil
		}
		for i := 0; i < size; i++ {
			high, low := splitByte(bs[i])
			wr.Write(encodeCodeMap[high])
			wr.Write(encodeCodeMap[low])
		}
	}
	return nil
}

func decode(r io.Reader, wr io.Writer) error {
	runeReader := bufio.NewReader(r)
	for {
		high, err := readHalfByte(runeReader)
		if err != nil {
			if err == io.EOF {
				break
			}
			return err
		}
		low, err := readHalfByte(runeReader)
		if err != nil {
			if err == io.EOF {
				break
			}
			return err
		}
		wr.Write([]byte{mergeByte(high, low)})
	}
	return nil
}

var RuneError = errors.New("read a wrong rune")

func readHalfByte(r io.RuneReader) (byte, error) {
	s, err := readRune(r, 2)
	if err != nil {
		return 0, err
	}
	if _, exist := codes[s]; !exist {
		return 0, fmt.Errorf("%s %w", s, RuneError)
	}
	if codes[s] >= 10 {
		s1, err := readRune(r, 2)
		if err != nil {
			return 0, nil
		}
		if _, exist := codes[s]; !exist {
			return 0, fmt.Errorf("%s %w", s, RuneError)
		}
		s += s1
	}
	return decodeCodeMap[s], nil
}

func splitByte(b byte) (byte, byte) {
	return (b & 0xf0) >> 4, b & 0x0f
}

func mergeByte(h, l byte) byte {
	return (h << 4) | l
}

func readRune(r io.RuneReader, size int) (string, error) {
	result := make([]rune, 0, size)
	for i := 0; i < size; i++ {
		ru, _, err := r.ReadRune()
		if err != nil {
			// 最后一个字符是回车不处理
			if i != 0 && result[len(result)-1] != 10 && err == io.EOF {
				return "", fmt.Errorf("input is short, %w", err)
			}
			return "", err
		}
		result = append(result, ru)
	}
	return string(result), nil
}
