#include <algorithm>
#include <chrono>
#include <cmath>
#include <cstdint>
#include <iostream>
#include <numeric>
#include <sstream>
#include <vector>

struct tree_t
{
private:
  struct node_t
  {
    node_t* parent;
    node_t* left;
    node_t* right;

    int64_t value;

    node_t(int64_t value_, node_t* left_ = nullptr, node_t* right_ = nullptr)
      : parent()
      , left(left_)
      , right(right_)
      , value(value_)
    {
      if (left)
        left->parent = this;
      if (right)
        right->parent = this;
    }
  };

  node_t* root;
  node_t* finger;

  void destroy(node_t* from)
  {
    if (!from)
      return;

    destroy(from->left);
    destroy(from->right);
    delete from;
  }

  node_t* build_full_internal(int depth, int64_t value)
  {
    if (depth == 0)
      return nullptr;

    node_t* left  = build_full_internal(depth - 1, value * 2);
    node_t* right = build_full_internal(depth - 1, value * 2 + 1);
    return new node_t(value, left, right);
  }

public:
  tree_t()
    : root()
    , finger()
  { }

  ~tree_t()
  {
    destroy(root);
  }

  void build_full(int depth)
  {
    destroy(root);
    root = finger = build_full_internal(depth, 1);
  }

  void finger_up()
  {
    if (!finger->parent)
      return;

    finger = finger->parent;
  }

  void finger_left()
  {
    if (!finger->left)
      return;

    finger = finger->left;
  }

  void finger_right()
  {
    if (!finger->right)
      return;

    finger = finger->right;
  }

  void finger_set(int64_t value)
  {
    finger->value = value;
  }

  void finger_reset()
  {
    finger = root;
  }
};

constexpr int64_t MOVE_UP = 0;
constexpr int64_t MOVE_LEFT = 1;
constexpr int64_t MOVE_RIGHT = 2;

// Convert finger movement instructions into node positions
// relative to the root.
template <typename ItIn, typename ItOut>
void convert(ItIn in_begin, ItIn in_end, ItOut out)
{
  std::vector<int64_t> stack;

  while (in_begin != in_end)
  {
    int64_t v = *in_begin++;
    switch (v)
    {
      case MOVE_UP:
        stack.pop_back();
        break;
      case MOVE_LEFT:
      case MOVE_RIGHT:
        stack.push_back(v);
        break;
      default:
        out = std::copy(stack.begin(), stack.end(), out);
        *out++ = v;
        break;
    }
  }
}

template <typename It>
void interpret_finger(tree_t& t, It begin, It end)
{
  while (begin != end)
  {
    int64_t v = *begin++;
    switch (v)
    {
      case MOVE_UP:
        t.finger_up();
        break;
      case MOVE_LEFT:
        t.finger_left();
        break;
      case MOVE_RIGHT:
        t.finger_right();
        break;
      default:
        t.finger_set(v);
        break;
    }
  }
}

template <typename It>
void interpret_root(tree_t& t, It begin, It end)
{
  while (begin != end)
  {
    int64_t v = *begin++;
    switch (v)
    {
      case MOVE_UP:
        break;
      case MOVE_LEFT:
        t.finger_left();
        break;
      case MOVE_RIGHT:
        t.finger_right();
        break;
      default:
        t.finger_set(v);
        t.finger_reset();
        break;
    }
  }
}

template <typename It>
std::pair<double, double> analyze(It begin, It end)
{
  int size = std::distance(begin, end);
  double mean = std::accumulate(begin, end, 0.0) / size;

  double sum_sq = 0.0;
  std::for_each(begin, end, [&] (double x) { sum_sq += (x - mean) * (x - mean); });
  double stddev = std::sqrt(sum_sq / (size - 1));

  return {mean, stddev};
}

int main(int argc, char** argv)
{
  using namespace std::chrono;

  int time_limit;

  {
    std::stringstream s(argv[1]);
    s >> time_limit;
  }

  std::vector<int64_t> in_finger;

  int depth;
  std::cin >> depth;

  {
    int64_t i;
    while (std::cin >> i)
      in_finger.push_back(i);
  }

  std::vector<int64_t> in_root;
  convert(in_finger.begin(), in_finger.end(), std::back_inserter(in_root));

  tree_t t;
  t.build_full(depth);

  {
    std::vector<double> finger_results;

    auto measurement_start = high_resolution_clock::now();
    for (int i = 1;; i++)
    {
      auto start = high_resolution_clock::now();
      for (int j = 0; j < i; j++)
      {
        t.finger_reset();
        interpret_finger(t, in_finger.begin(), in_finger.end());
      }
      auto end = high_resolution_clock::now();
      duration<double, std::milli> diff = end - start;
      finger_results.push_back(diff.count() / i);
      if (duration_cast<milliseconds>(end - measurement_start).count() > time_limit)
        break;
    }

    auto result = analyze(finger_results.begin(), finger_results.end());
    std::cout << "finger_mean = " << result.first << " ms\n";
    std::cout << "finger_stddev = " << result.second << " ms\n";
    std::cout << "finger_results = ";
    for (auto d : finger_results)
      std::cout << d << ",";
    std::cout << "\n";
  }

  {
    std::vector<double> root_results;

    auto measurement_start = high_resolution_clock::now();
    for (int i = 1;; i++)
    {
      auto start = high_resolution_clock::now();
      for (int j = 0; j < i; j++)
      {
        t.finger_reset();
        interpret_root(t, in_root.begin(), in_root.end());
      }
      auto end = high_resolution_clock::now();
      duration<double, std::milli> diff = end - start;
      root_results.push_back(diff.count() / i);
      if (duration_cast<milliseconds>(end - measurement_start).count() > time_limit)
        break;
    }

    auto result = analyze(root_results.begin(), root_results.end());
    std::cout << "root_mean = " << result.first << " ms\n";
    std::cout << "root_stddev = " << result.second << " ms\n";
    std::cout << "root_results = ";
    for (auto d : root_results)
      std::cout << d << ",";
    std::cout << "\n";
  }
}
