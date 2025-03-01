#pragma once

#include <algorithm>
#include <initializer_list>
#include <memory>
#include <utility>

namespace ipd {

// A simple binary search tree using leaf-insertion (no balance).
template<typename T>
class Bst
{
public:
    // Constructs an empty tree.
    Bst();
    // Constructs a tree containing the given elements.
    Bst(std::initializer_list<T>);

    // Is this tree empty?
    bool empty() const;
    // Returns the number of elements in the tree.
    size_t size() const;

    // Does the tree contain the given value?
    bool contains(const T&) const;
    // Inserts an element into the tree, if absent.
    void insert(const T&);
    // Removes an element from the tree, if present.
    void remove(const T&);

private:
    struct node_;
    using ptr_ = std::unique_ptr<node_>;

    ptr_   root_;
    size_t size_;
};

template<typename T>
struct Bst<T>::node_
{
    node_(const T& value)
            : data(value), left(nullptr), right(nullptr) {}

    T      data;
    ptr_   left;
    ptr_   right;
};

template<typename T>
Bst<T>::Bst()
        : root_(nullptr), size_(0) {}

template<typename T>
Bst<T>::Bst(std::initializer_list<T> init)
        : Bst()
{
    for (const auto& each : init)
        insert(each);
}

template<typename T>
bool Bst<T>::empty() const
{
    return size_ == 0;
}

template<typename T>
size_t Bst<T>::size() const
{
    return size_;
}

template<typename T>
bool Bst<T>::contains(const T& key) const
{
    node_* curr = &*root_;

    while (curr != nullptr) {
        if (key < curr->data) curr = &*curr->left;
        else if (key > curr->data) curr = &*curr->right;
        else return true;
    }

    return false;
}

template<typename T>
void Bst<T>::insert(const T& key)
{
    ptr_* curr = &root_;

    while (*curr != nullptr) {
        if (key < (*curr)->data) curr = &(*curr)->left;
        else if (key > (*curr)->data) curr = &(*curr)->right;
        else return;
    }

    *curr = std::make_unique<node_>(key);
    ++size_;
}

template<typename T>
void Bst<T>::remove(const T& key)
{
    ptr_* curr = &root_;

    while (*curr != nullptr) {
        if (key < (*curr)->data) curr = &(*curr)->left;
        else if (key > (*curr)->data) curr = &(*curr)->right;
        else {
            // To remove a node that has no right child, we replace it with
            // its left child. Otherwise, we find the successor node by going
            // to the right once and to the left as far as possible. Then we
            // swap the contents with the successor node and delete the
            // successor by replacing it with its right child.
            if ((*curr)->right == nullptr) {
                *curr = std::move((*curr)->left);
            } else {
                ptr_* succ = &(*curr)->right;
                while ((*succ)->left != nullptr)
                    succ = &(*succ)->left;
                std::swap((*succ)->data, (*curr)->data);
                *succ = std::move((*succ)->right);
            }

            size_--;
            return;
        }
    }
}

}
