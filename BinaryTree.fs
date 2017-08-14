module BinaryTree

type BinaryTree<'LeafData> =
    | Empty
    | TreeNode of 'LeafData * BinaryTree<'LeafData> * BinaryTree<'LeafData>
    