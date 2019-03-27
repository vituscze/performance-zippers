#include <algorithm>
#include <array>
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
    std::array<int64_t, 2> values;
    std::array<node_t*, 3> children;
    node_t* parent;
    bool is_two_node;

    node_t(int64_t value,
           node_t* child0 = nullptr,
           node_t* child1 = nullptr) :
      values(),
      children(),
      parent(),
      is_two_node(true)
    {
      values[0] = value;
      set_child(0, child0);
      set_child(1, child1);
    }

    node_t(int64_t value1,
           int64_t value2,
           node_t* child0 = nullptr,
           node_t* child1 = nullptr,
           node_t* child2 = nullptr) :
      values(),
      children(),
      parent(),
      is_two_node(false)
    {
      values[0] = value1;
      values[1] = value2;
      set_child(0, child0);
      set_child(1, child1);
      set_child(2, child2);
    }

    void set_child(int64_t i, node_t* new_child)
    {
      if (new_child)
        new_child->parent = this;

      children[i] = new_child;
    }

    bool is_data_node() const
    {
      return children[0] == nullptr;
    }

    bool contains(int64_t value) const
    {
      if (is_two_node)
        return values[0] == value;
      else
        return values[0] == value || values[1] == value;
    }

    // Extend the node with a new value and subtree.
    //
    // If the node wasn't full, return {0, nullptr}.
    // If the node was full, create a new node, distribute the
    // values and subtrees between the two nodes and return the
    // overflowed value and the right node.
    std::pair<int64_t, node_t*> add(int64_t value, node_t* new_node)
    {
      if (is_two_node)
      {
        if (value < values[0])
        {
          values[1] = values[0];
          values[0] = value;
          children[2] = children[1];
          set_child(1, new_node);
        }
        else
        {
          values[1] = value;
          set_child(2, new_node);
        }

        is_two_node = false;
        return {0, nullptr};
      }
      else
      {
        int64_t overflow_value;
        node_t* overflow_node;

        if (is_data_node())
        {
          if (value < values[0])
          {
            overflow_value = values[0];
            overflow_node = new node_t(values[0], values[1]);
            values[0] = value;
          }
          else if (value < values[1])
          {
            overflow_value = value;
            overflow_node = new node_t(value, values[1]);
          }
          else
          {
            overflow_value = values[1];
            overflow_node = new node_t(values[1], value);
          }
        }
        else
        {
          if (value < values[0])
          {
            overflow_value = values[0];
            overflow_node = new node_t(values[1], children[1], children[2]);
            values[0] = value;
            set_child(1, new_node);
          }
          else if (value < values[1])
          {
            overflow_value = value;
            overflow_node = new node_t(values[1], new_node, children[2]);
          }
          else
          {
            overflow_value = values[1];
            overflow_node = new node_t(value, children[2], new_node);
          }
        }

        children[2] = nullptr;
        is_two_node = true;
        return {overflow_value, overflow_node};
      }
    }
  };

  node_t* root;
  node_t* last_inserted;

  void destroy(node_t* from)
  {
    if (!from)
      return;

    for (auto& n : from->children)
      destroy(n);

    delete from;
  }

  node_t* find(int64_t value) const
  {
    node_t* current = nullptr;
    node_t* candidate = root;

    while (candidate)
    {
      current = candidate;

      if (value < current->values[0])
        candidate = current->children[0];
      else if (current->is_two_node || value < current->values[1])
        candidate = current->children[1];
      else
        candidate = current->children[2];
    }

    return current;
  }

  void insert_from(node_t* from, int64_t value)
  {
    if (!from)
    {
      root = last_inserted = new node_t(value);
      return;
    }

    if (from->contains(value))
    {
      last_inserted = from;
      return;
    }

    node_t* current = from;
    node_t* candidate = from->parent;

    auto result = current->add(value, nullptr);
    last_inserted = current->contains(value) ? current : result.second;

    while (candidate && result.second)
    {
      current = candidate;
      result = current->add(result.first, result.second);
      candidate = current->parent;
    }

    if (!candidate && result.second)
      root = new node_t(result.first, current, result.second);
  }

public:
  tree_t() :
    root(),
    last_inserted()
  { }

  ~tree_t()
  {
    destroy(root);
  }

  void insert(int64_t value)
  {
    insert_from(find(value), value);
  }

  void insert_last(int64_t value)
  {
    insert_from(last_inserted, value);
  }
};

template <typename It>
std::pair<double, double> analyze(It begin, It end)
{
  int size = std::distance(begin, end);

  double sum = std::accumulate(begin, end, 0.0);
  double mean = sum / size;

  double sum_sq = 0.0;
  std::for_each(begin, end, [&] (double x) { sum_sq += (x - mean) * (x - mean); });
  double stddev = std::sqrt(sum_sq / (size - 1));

  return {mean, stddev};
}

int main(int argc, char** argv)
{
  using namespace std::chrono;
  constexpr int SIZE = 10000000;

  int time_limit;

  {
    std::stringstream s(argv[1]);
    s >> time_limit;
  }

  {
    std::vector<double> finger_results;

    auto measurement_start = high_resolution_clock::now();
    for (int i = 1;; i++)
    {
      auto start = high_resolution_clock::now();
      for (int j = 0; j < i; j++)
      {
        tree_t t;
        for (int64_t k = SIZE; k >= 0; k--)
          t.insert_last(k);
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
        tree_t t;
        for (int64_t k = SIZE; k >= 0; k--)
          t.insert(k);
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
