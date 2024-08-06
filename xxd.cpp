#include <algorithm>
using std::transform;
#include <fstream>
using std::ifstream;
#include <iomanip>
using std::hex;
using std::left;
using std::right;
using std::setfill;
using std::setw;
#include <iostream>
using std::cerr;
using std::cin;
using std::cout;
using std::endl;
using std::istream;
#include <iterator>
using std::ostream_iterator;
#include <memory>
using std::unique_ptr;
#include <regex>
using std::regex;
using std::regex_replace;
#include <sstream>
using std::ostringstream;
#include <string>
using std::string;

#define DEFAULT_WIDTH 16


void xxd(istream& input, int bufferSize=DEFAULT_WIDTH) {
    unique_ptr<char[]> buffer(new char[bufferSize]);

    int address = 0;
    while (input) {
        input.read(buffer.get(), bufferSize);
        string hexdump(buffer.get(), input.gcount());

        // Log byte-buffer address
        cout << hex << right << setw(8) << setfill('0') << address << ": ";

        // Log byte-buffer in hexadecimal
        bool toggle = false;
        ostringstream dumpss;
        transform(hexdump.begin(), hexdump.end(),
                ostream_iterator<string>(dumpss, ""),
                [&toggle](unsigned char c) { 
                    toggle = !toggle;
                    ostringstream oss;
                    oss << hex << setw(2) << setfill('0') << static_cast<int>(c)
                        << (toggle ? "" : " "); // only pad every other byte
                    return oss.str();
                });

        // Log byte-buffer in a limited subset of ASCII (for readability)
        //TODO: support -c/--col CLI argument for more dynamic widths ("40" harcoded for bufferSize 16)
        cout << left << setfill(' ') << setw(40) << dumpss.str() << ' ';
        cout << regex_replace(hexdump, regex("[^ -~]"), ".") << endl; // Replace all non-readable ascii (outside 0x20 - 0x7e) with a period
        address += bufferSize;
    }
}


int main(int argc, char **argv) {

    // Use file from CLI
    if (argc > 1) {
        ifstream file(argv[1]);
        if (!file) {
            cerr << "Failed to open file: " << argv[1] << endl;
            return EXIT_FAILURE;
        }
        xxd(file);

    // Use stdin
    } else {
        xxd(cin);
    }

    return EXIT_SUCCESS;
}
