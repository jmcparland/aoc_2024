module Dijkstra (
    runDijkstra,
) where

import Data.Heap qualified as H
import Data.Map.Strict qualified as M
import Data.Set qualified as S

type VisitedSet node = S.Set node
type PriorityQueue score node = H.Heap (score, node)
type BestEstimates node score = M.Map node score
type Predecessors node = M.Map node (S.Set node)

type NeighborFunction node = node -> [node]
type CostFunction node score = node -> node -> score

runDijkstra ::
    (Ord node, Ord score, Num score) =>
    NeighborFunction node ->
    CostFunction node score ->
    BestEstimates node score ->
    (BestEstimates node score, Predecessors node)
runDijkstra neighbors cost initialBest =
    dijkstra initialBest M.empty S.empty (heapFromBest initialBest)
  where
    dijkstra best preds visited heap =
        case H.viewMin heap of
            Nothing -> (best, preds)
            Just ((c, point), heap') ->
                if point `S.member` visited
                    then dijkstra best preds visited heap'
                    else
                        let nghs = neighbors point
                            (newBest, newPreds) = updateBestAndPreds best preds c point nghs
                            newHeap = updateHeap heap' c point nghs
                            newVisited = S.insert point visited
                         in dijkstra newBest newPreds newVisited newHeap

    updateBestAndPreds best preds c point nghs =
        foldl (update c point) (best, preds) nghs

    update c point (best', preds') n =
        let newCost = c + cost point n
            oldCost = M.lookup n best'
         in case oldCost of
                Nothing ->
                    let newBest = M.insert n newCost best'
                        newPreds = M.insert n (S.singleton point) preds'
                     in (newBest, newPreds)
                Just old
                    | newCost < old ->
                        let newBest = M.insert n newCost best'
                            newPreds = M.insert n (S.singleton point) preds'
                         in (newBest, newPreds)
                    | newCost == old ->
                        let newPreds = M.insert n (S.insert point (M.findWithDefault S.empty n preds')) preds'
                         in (best', newPreds)
                    | otherwise -> (best', preds')

    updateHeap heap c point nghs =
        foldl (\h q -> H.insert (c + cost point q, q) h) heap nghs

    heapFromBest best = H.fromList $ map (\(point, c) -> (c, point)) $ M.toList best