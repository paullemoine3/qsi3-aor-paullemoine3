type player =
  | PlayerOne
  | PlayerTwo;

type point =
  | Love
  | Fifteen
  | Thirty;

type pointsData = {
  playerOne: point,
  playerTwo: point,
};

type fortyData = {
  player, /* The player who have forty points */
  otherPlayerPoint: point,
};

type score =
  | Points(pointsData)
  | Forty(fortyData)
  | Deuce
  | Advantage(player)
  | Game(player);

let scoreWhenDeuce: player => score = winner => Advantage(winner);

let scoreWhenAdvantage: (player, player) => score =
  (advantagedPlayer, winner) =>
    advantagedPlayer == winner ? Game(winner) : Deuce;

let other = player =>
  switch (player) {
  | PlayerOne => PlayerTwo
  | PlayerTwo => PlayerOne
  };

let incrementPoint: point => option(point) =
  point =>
    switch (point) {
    | Love => Some(Fifteen)
    | Fifteen => Some(Thirty)
    | Thirty => None
    };

let scoreWhenForty = (current, winner) =>
  current.player == winner ?
    Game(winner) :
    (
      switch (incrementPoint(current.otherPlayerPoint)) {
      | Some(p) => Forty({...current, otherPlayerPoint: p})
      | None => Deuce
      }
    );

let pointTo = (player, point, current) =>
  switch (player) {
  | PlayerOne => {...current, playerOne: point}
  | PlayerTwo => {...current, playerTwo: point}
  };

let pointFor = (player, current) =>
  switch (player) {
  | PlayerOne => current.playerOne
  | PlayerTwo => current.playerTwo
  };

let scoreWhenPoints = (current, winner) =>
  switch (current |> pointFor(winner) |> incrementPoint) {
  | Some(np) => Points(pointTo(winner, np, current))
  | None =>
    Forty({
      player: winner,
      otherPlayerPoint: current |> pointFor(other(winner)),
    })
  };

let scoreWhenGame = winner => Game(winner);

let score = (current, winner) =>
  switch (current) {
  | Points(p) => scoreWhenPoints(p, winner)
  | Forty(f) => scoreWhenForty(f, winner)
  | Deuce => scoreWhenDeuce(winner)
  | Advantage(a) => scoreWhenAdvantage(a, winner)
  | Game(g) => scoreWhenGame(g)
  };

let newGame = Points({playerOne: Love, playerTwo: Love});

let string_of_player: player => string =
  somePlayer =>
    switch (somePlayer) {
    | PlayerOne => "Player 1"
    | PlayerTwo => "Player 2"
    };

let string_of_point: point => string =
  somePoint =>
    switch (somePoint) {
    | Love => "0"
    | Fifteen => "15"
    | Thirty => "30"
    };

let string_of_score: score => string =
  someScore =>
    switch (someScore) {
    | Points(pointData) =>
      "Player 1: "
      ++ string_of_point(pointData.playerOne)
      ++ "-"
      ++ string_of_point(pointData.playerTwo)
      ++ " : Player 2"
    | Forty(fortyData) =>
      switch (fortyData.player) {
      | PlayerOne =>
        "40" ++ " - " ++ string_of_point(fortyData.otherPlayerPoint)
      | PlayerTwo =>
        string_of_point(fortyData.otherPlayerPoint) ++ " - " ++ "40"
      }
    | Deuce => "Player 1 : 40" ++ "-" ++ "40 : Player 2"
    | Advantage(player) => "Advantage for : " ++ string_of_player(player)
    | Game(player) => "Game for : " ++ string_of_player(player)
    };