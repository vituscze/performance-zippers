using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

namespace Tree23
{
    class Tree
    {
        private class TreeNode
        {
            public Int64 value0;
            public Int64 value1;
            public TreeNode child0;
            public TreeNode child1;
            public TreeNode child2;
            public TreeNode parent;
            public bool isTwoNode;

            public TreeNode(Int64 value0_, TreeNode child0_ = null, TreeNode child1_ = null)
            {
                isTwoNode = true;
                parent = null;

                value0 = value0_;
                value1 = 0;
                SetChild(0, child0_);
                SetChild(1, child1_);
                SetChild(2, null);
            }

            public TreeNode(Int64 value0_, Int64 value1_, TreeNode child0_ = null, TreeNode child1_ = null, TreeNode child2_ = null)
            {
                isTwoNode = false;
                parent = null;

                value0 = value0_;
                value1 = value1_;
                SetChild(0, child0_);
                SetChild(1, child1_);
                SetChild(2, child2_);
            }

            public void SetChild(Int64 index, TreeNode newChild)
            {
                switch (index)
                {
                    case 0: child0 = newChild; break;
                    case 1: child1 = newChild; break;
                    case 2: child2 = newChild; break;
                }

                if (newChild != null)
                {
                    newChild.parent = this;
                }
            }

            public bool IsDataNode()
            {
                return child0 == null;
            }

            public bool Contains(Int64 value)
            {
                if (isTwoNode)
                {
                    return value0 == value;
                }
                else
                {
                    return value0 == value || value1 == value;
                }
            }

            public KeyValuePair<Int64, TreeNode> Add(Int64 value, TreeNode newNode)
            {
                if (isTwoNode)
                {
                    if (value < value0)
                    {
                        value1 = value0;
                        value0 = value;
                        child2 = child1;
                        SetChild(1, newNode);
                    }
                    else
                    {
                        value1 = value;
                        SetChild(2, newNode);
                    }

                    isTwoNode = false;
                    return new KeyValuePair<Int64, TreeNode>(0, null);
                }
                else
                {
                    Int64 overflowValue = 0;
                    TreeNode overflowNode = null;

                    if (IsDataNode())
                    {
                        if (value < value0)
                        {
                            overflowValue = value0;
                            overflowNode = new TreeNode(value0, value1);
                            value0 = value;
                        }
                        else if (value < value1)
                        {
                            overflowValue = value;
                            overflowNode = new TreeNode(value, value1);
                        }
                        else
                        {
                            overflowValue = value1;
                            overflowNode = new TreeNode(value1, value);
                        }
                    }
                    else
                    {
                        if (value < value0)
                        {
                            overflowValue = value0;
                            overflowNode = new TreeNode(value1, child1, child2);
                            value0 = value;
                            SetChild(1, newNode);
                        }
                        else if (value < value1)
                        {
                            overflowValue = value;
                            overflowNode = new TreeNode(value1, newNode, child2);
                        }
                        else
                        {
                            overflowValue = value1;
                            overflowNode = new TreeNode(value, child2, newNode);
                        }
                    }

                    child2 = null;
                    isTwoNode = true;
                    return new KeyValuePair<Int64, TreeNode>(overflowValue, overflowNode);
                }
            }
        }

        private TreeNode root;
        private TreeNode lastInserted;

        private TreeNode Find(Int64 value)
        {
            TreeNode current = null;
            TreeNode candidate = root;

            while (candidate != null)
            {
                current = candidate;

                if (value < current.value0)
                {
                    candidate = current.child0;
                }
                else if (current.isTwoNode || value < current.value1)
                {
                    candidate = current.child1;
                }
                else
                {
                    candidate = current.child2;
                }
            }

            return current;
        }

        private void InsertFrom(TreeNode from, Int64 value)
        {
            if (from == null)
            {
                root = lastInserted = new TreeNode(value);
                return;
            }

            if (from.Contains(value))
            {
                lastInserted = from;
                return;
            }


            TreeNode current = from;
            TreeNode candidate = from.parent;

            var result = current.Add(value, null);
            lastInserted = current.Contains(value) ? current : result.Value;

            while (candidate != null && result.Value != null)
            {
                current = candidate;
                result = current.Add(result.Key, result.Value);
                candidate = current.parent;
            }

            if (candidate == null && result.Value != null)
            {
                root = new TreeNode(result.Key, current, result.Value);
            }
        }

        public Tree()
        { }

        public void Insert(Int64 value)
        {
            InsertFrom(Find(value), value);
        }

        public void InsertLast(Int64 value)
        {
            InsertFrom(lastInserted, value);
        }
    }

    class Program
    {
        static KeyValuePair<double, double> Analyze(List<double> input)
        {
            double mean = input.Average();

            double sumSq = 0.0;
            input.ForEach(x => sumSq += (x - mean) * (x - mean));
            double stddev = Math.Sqrt(sumSq / (input.Count - 1));

            return new KeyValuePair<double, double>(mean, stddev);
        }

        static void Main(string[] args)
        {
            const int MAX_ITER = 100;
            const int SIZE = 10000000;

            Stopwatch s = new Stopwatch();

            {
                List<double> fingerResults = new List<double>();

                for (int i = 0; i < MAX_ITER; i++)
                {
                    s.Reset();
                    s.Start();
                    {
                        Tree t = new Tree();
                        for (Int64 k = SIZE; k >= 0; k--)
                        {
                            t.InsertLast(k);
                        }
                    }
                    s.Stop();
                    fingerResults.Add(s.ElapsedMilliseconds);
                }

                var result = Analyze(fingerResults);
                Console.WriteLine("finger_mean = {0} ms", result.Key);
                Console.WriteLine("finger_stddev = {0} ms", result.Value);
            }

            {
                List<double> rootResults = new List<double>();

                for (int i = 0; i < MAX_ITER; i++)
                {
                    s.Reset();
                    s.Start();
                    {
                        Tree t = new Tree();
                        for (Int64 k = SIZE; k >= 0; k--)
                        {
                            t.Insert(k);
                        }
                    }
                    s.Stop();
                    rootResults.Add(s.ElapsedMilliseconds);
                }

                var result = Analyze(rootResults);
                Console.WriteLine("root_mean = {0} ms", result.Key);
                Console.WriteLine("root_stddev = {0} ms", result.Value);
            }
        }
    }
}
