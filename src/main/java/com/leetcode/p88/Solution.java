package com.leetcode.p88;

class Solution {
  public void merge(int[] nums1, int m, int[] nums2, int n) {
    if (nums2.length == 0) return;
    if (nums1.length == n) {
      System.arraycopy(nums2, 0, nums1, 0, nums2.length);
      return;
    }
    int[] res = new int[nums1.length];
    for (int i1 = 0, i2 = 0, r = 0; r < res.length; r++) {
      if (i1 >= m) {
        res[r] = nums2[i2++];
      } else if (i2 >= n) {
        res[r] = nums1[i1++];
      } else {
        res[r] = nums1[i1] < nums2[i2]
            ? nums1[i1++]
            : nums2[i2++];
      }
    }
    System.arraycopy(res, 0, nums1, 0, res.length);
  }

  public static void main(String[] args) {
    int[] n1 = new int[2];
    n1[0] = 2;

    new Solution().merge(n1, 1, new int[]{1}, 1);
  }
}
