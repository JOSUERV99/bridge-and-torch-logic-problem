// constants
const initialSide = 0, otherSide = 1, timeSide = 2;

// problems params
const timeAvailable = 21;
const peopleAtTheSameTime = 3;
const people = [
    {  name: 'A', time: 1,  },
    {  name: 'B', time: 2,  },
    {  name: 'C', time: 5,  },
    {  name: 'D', time: 10, },
    {  name: 'E', time: 15, },
];

// initial state
let initialState = [
    people,
    [],
    timeAvailable,
];

// final state
const finalState = [
    [],
    people,
    checkTime => checkTime >= 0,
];

///////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Objective: implement the basics to solve
// the crossing the bridge problem
/*
Functions:
    final_state
    move
    update
    legal
    initial_state
    value	solo para hill-climbing y best-first
*/

// Utils //////////////////////////////////////////////////////////////////////////////////////////////////////
// select initialState[timeSide] different people
const selectPeople = (n, people, res) => {
    if (n == 0)
        return res;
    else
    {
        for ( let person of people ) {
            if ( !res.find( e => e === person ) ) {
                res.push(person);
                return selectPeople(n-1, people.filter(e => e !== person ), res);
            }
        }
        return res;
    }
}

const depth_first = (initialState) => {
    
    let currentPeople = selectPeople(peopleAtTheSameTime, initialState[initialSide], []); 
    currentPeople.sort( (a, b) => a.time - b.time );

    console.log(currentPeople)


};

depth_first(initialState);


