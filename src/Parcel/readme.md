brew install cmake
g++ parcel/cpp/test.cpp -Iparcel/cpp/SimpleData.hpp -Iparcel/include/include -std=c++11 -o test.out && ./test.out