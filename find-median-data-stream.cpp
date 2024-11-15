#include <algorithm>
#include <vector>
using std::pop_heap, std::push_heap;
const auto max_heap = std::less<int>{};
const auto min_heap = std::greater<int>{};

template <typename F>
std::pair<int, int> minmax(int a, int b, F cmp) {
    if (cmp(a, b)) { return {a, b}; }
    else { return {b, a}; }
}
template <typename F>
int shift_heap(std::vector<int>& v, int x, F cmp) {
    pop_heap(v.begin(), v.end(), cmp);
    // 最小堆用的 greater,后面的值会被输出,前面的值会被还入堆,最大堆同理
    auto [back, front] = minmax(x, v.back(), cmp);
    v.back() = back;
    push_heap(v.begin(), v.end(), cmp);
    return front;
}

template <typename F>
void push_heap(std::vector<int>& v, int x, F cmp) {
    v.push_back(x);
    push_heap(v.begin(), v.end(), cmp);
}

class MedianFinder {
public:
    std::vector<int> smallers;
    std::vector<int> greaters;
    MedianFinder() {
        smallers.reserve(100000);
        greaters.reserve(100000);
    }

    void addNum(int num) {
        if (smallers.empty() && greaters.empty()) {
            smallers.push_back(num);
            return;
        }
        if (greaters.empty()) {
            if (num >= smallers.front()) { greaters.push_back(num); }
            else {
                auto m = shift_heap(smallers, num, max_heap);
                greaters.push_back(m);
            }
            return;
        }
        if (smallers.empty()) {
            if (num <= greaters.front()) { smallers.push_back(num); }
            else {
                auto m = shift_heap(greaters, num, min_heap);
                smallers.push_back(m);
            }
            return;
        }
        if (smallers.size() < greaters.size()) {
            if (num <= greaters.front()) { push_heap(smallers, num, max_heap); }
            else {
                auto m = shift_heap(greaters, num, min_heap);
                push_heap(smallers, m, max_heap);
            }
            return;
        }
        if (num >= smallers.front()) { push_heap(greaters, num, min_heap); }
        else {
            auto m = shift_heap(smallers, num, max_heap);
            push_heap(greaters, m, min_heap);
        }
    }

    double findMedian() {
        if (smallers.size() == greaters.size()) {
            return (double)(smallers.front() + greaters.front()) / 2;
        }
        if (greaters.size() > smallers.size()) { return greaters.front(); }
        return smallers.front();
    }
};
