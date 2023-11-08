// Given an array nums, find all triplets such that:
// * nums[i] + nums[j] + nums[k] == 0
// * i != j
// * i != k
// * j != k
// and return only distinct triplets.
let findTriplets (nums: int array) =
    let idxList = [ 0 .. nums.Length - 1 ]
    // Cartesian product of all indexes as triplets: (i, j, k)
    let allPairs = 
        List.allPairs idxList idxList 
        |> List.allPairs idxList 
        |> List.map (fun (a, (b, c)) -> a, b, c)

    let addsToZeroAndIsDistinct (i, j, k) =
        nums[i] + nums[j] + nums[k] = 0 
        && i <> j 
        && i <> k 
        && j <> k

    let idxToVals (i, j, k) = [ nums[i]; nums[j]; nums[k] ] |> List.sort

    allPairs 
    |> List.filter addsToZeroAndIsDistinct
    |> List.map idxToVals
    |> List.distinct

findTriplets [| -1; 0; 1; 2; -1; 4 |];;