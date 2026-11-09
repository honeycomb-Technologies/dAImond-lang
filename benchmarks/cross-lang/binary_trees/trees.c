/* Binary Trees Benchmark
 * Allocate and traverse complete binary trees of depth 21
 * Tests allocation/deallocation performance
 */

#include <stdio.h>
#include <stdlib.h>

typedef struct TreeNode {
    struct TreeNode *left;
    struct TreeNode *right;
} TreeNode;

TreeNode *make_tree(int depth) {
    TreeNode *node = (TreeNode *)malloc(sizeof(TreeNode));
    if (depth > 0) {
        node->left = make_tree(depth - 1);
        node->right = make_tree(depth - 1);
    } else {
        node->left = NULL;
        node->right = NULL;
    }
    return node;
}

long count_nodes(TreeNode *node) {
    if (node == NULL) {
        return 0;
    }
    return 1 + count_nodes(node->left) + count_nodes(node->right);
}

void free_tree(TreeNode *node) {
    if (node == NULL) {
        return;
    }
    free_tree(node->left);
    free_tree(node->right);
    free(node);
}

int main(void) {
    int max_depth = 21;
    int iterations = 10;
    long count = 0;

    for (int i = 0; i < iterations; i++) {
        TreeNode *tree = make_tree(max_depth);
        count = count_nodes(tree);
        free_tree(tree);
    }

    printf("nodes: %ld\n", count);
    printf("iterations: %d\n", iterations);
    return 0;
}
