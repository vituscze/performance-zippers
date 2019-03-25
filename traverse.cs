using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

namespace Zipper
{
    class Tree
    {
        private class TreeNode
        {
            public TreeNode parent;
            public TreeNode left;
            public TreeNode right;
            public Int64 value;

            public TreeNode(Int64 value_, TreeNode left_ = null, TreeNode right_ = null)
            {
                parent = null;
                left = left_;
                if (left != null)
                    left.parent = this;
                right = right_;
                if (right != null)
                    right.parent = this;
                value = value_;
            }
        }

        private TreeNode root;
        private TreeNode finger;

        private TreeNode BuildFullInternal(Int64 depth, Int64 value)
        {
            if (depth == 0)
            {
                return null;
            }

            TreeNode left = BuildFullInternal(depth - 1, value * 2);
            TreeNode right = BuildFullInternal(depth - 1, value * 2 + 1);
            return new TreeNode(value, left, right);
        }

        public Tree()
        { }

        public void BuildFull(int depth)
        {
            root = finger = BuildFullInternal(depth, 1);
        }

        public void FingerUp()
        {
            if (finger.parent == null)
            {
                return;
            }

            finger = finger.parent;
        }

        public void FingerLeft()
        {
            if (finger.left == null)
            {
                return;
            }

            finger = finger.left;
        }

        public void FingerRight()
        {
            if (finger.right == null)
            {
                return;
            }

            finger = finger.right;
        }

        public void FingerSet(Int64 value)
        {
            finger.value = value;
        }

        public void FingerReset()
        {
            finger = root;
        }
    }

    class Program
    {
        static void Convert(List<Int64> input, List<Int64> output)
        {
            Stack<Int64> stack = new Stack<Int64>();

            foreach (var v in input)
            {
                switch (v)
                {
                    case 0:
                        stack.Pop();
                        break;
                    case 1:
                    case 2:
                        stack.Push(v);
                        break;
                    default:
                        output.AddRange(stack);
                        output.Add(v);
                        break;
                }
            }
        }

        static void InterpretFinger(Tree t, List<Int64> input)
        {
            foreach (var v in input)
            {
                switch (v)
                {
                    case 0:
                        t.FingerUp();
                        break;
                    case 1:
                        t.FingerLeft();
                        break;
                    case 2:
                        t.FingerRight();
                        break;
                    default:
                        t.FingerSet(v);
                        break;
                }
            }
        }

        static void InterpretRoot(Tree t, List<Int64> input)
        {
            foreach (var v in input)
            {
                switch (v)
                {
                    case 0:
                        break;
                    case 1:
                        t.FingerLeft();
                        break;
                    case 2:
                        t.FingerRight();
                        break;
                    default:
                        t.FingerSet(v);
                        t.FingerReset();
                        break;
                }
            }
        }

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

            List<Int64> inFinger = new List<Int64>();

            int depth = int.Parse(Console.ReadLine());

            {
                Int64 i;
                while (Int64.TryParse(Console.ReadLine(), out i))
                {
                    inFinger.Add(i);
                }
            }

            List<Int64> inRoot = new List<Int64>();
            Convert(inFinger, inRoot);

            Tree t = new Tree();
            t.BuildFull(depth);

            Stopwatch s = new Stopwatch();

            {
                List<double> fingerResults = new List<double>();

                for (int i = 0; i < MAX_ITER; i++)
                {
                    s.Reset();
                    s.Start();
                    t.FingerReset();
                    InterpretFinger(t, inFinger);
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
                    t.FingerReset();
                    InterpretRoot(t, inRoot);
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
