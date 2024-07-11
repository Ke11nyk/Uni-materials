#pragma once
#include "Edge.h"
#include <iostream>
#include <vector>
#include <limits>
#include <queue>
#include <stack>
#include <thread>
#include <future>
#include <mutex>
#include <chrono>
#include <fstream>
#include <random>
#include <algorithm>

#define lng long long

/// <summary>
/// Basic class of graph
/// </summary>
class Graph {
protected:
    lng V;
    std::vector<Edge> edges;
    /// <summary>
    /// Graph adjacency list
    /// </summary>
    std::vector<std::vector<std::pair<lng, lng>>> adj_list;

    /// <summary>
    /// Constructor of the graph
    /// </summary>
    /// <param name="edges">Vector of edges</param>
    /// <param name="V">Number of vertices</param>
    Graph(std::vector<Edge>& edges, lng V) : edges(edges), V(V) {
        adj_list.resize(V + 1);
        for (const auto& edge : edges)
            adj_list[edge.from].emplace_back(edge.to, edge.weight);
    }

    std::vector<lng> BellmanFord(lng& V, std::vector<Edge>& edges);

    std::vector<lng> Dijkstra(lng src, std::vector<std::vector<std::pair<lng, lng>>>& adj_list, 
        lng V, std::vector<std::vector<lng>>& paths);

public:
    void printGraph();

    /// <summary>
    /// Johnson's algorithm 
    /// </summary>
    /// <param name="E">Number of edges</param>
    /// <param name="paths">Pathes from each vertex to each vertex</param>
    /// <returns>Distances(weight) of the shortest paths</returns>
    virtual std::vector<std::vector<lng>> Johnson(lng E, std::vector<std::vector<lng>>& paths) = 0;
};
