open Jest;

open Expect;

open Tennis;

describe("Test tennis transitions", () => {
  test("Given deuce when player wins then score is correct", () =>
    expect(Tennis.scoreWhenDeuce(PlayerOne))
    |> toEqual(Advantage(PlayerOne))
  );
  test("Given advantage when advantaged player wins then score is correct", () => {
    let advantagedPlayer = PlayerOne;
    let winner = advantagedPlayer;
    expect(Tennis.scoreWhenAdvantage(advantagedPlayer, winner))
    |> toEqual(Game(advantagedPlayer));
  });
  test("Given advantage when the other player wins then score is Deuce", () => {
    let advantagedPlayer = PlayerOne;
    let winner = other(advantagedPlayer);
    expect(Tennis.scoreWhenAdvantage(advantagedPlayer, winner))
    |> toEqual(Deuce);
  });
  test("Given player: 40 when wins then score is Game for this player", () => {
    let fortyThirty = {player: PlayerOne, otherPlayerPoint: Thirty};
    expect(scoreWhenForty(fortyThirty, fortyThirty.player))
    |> toEqual(Game(fortyThirty.player));
  });
  test("Given player: 40 | other : 30 when other wins then score is Deuce", () => {
    let fortyThirty = {player: PlayerOne, otherPlayerPoint: Thirty};
    expect(scoreWhenForty(fortyThirty, other(fortyThirty.player)))
    |> toEqual(Deuce);
  });
  test("Given player: 40 | other < 30 when other wins then score is Deuce", () => {
    let fortyLove = {player: PlayerOne, otherPlayerPoint: Love};
    let fortyFifteen = {player: PlayerOne, otherPlayerPoint: Fifteen};
    expect(scoreWhenForty(fortyLove, other(fortyLove.player)))
    |> toEqual(Forty(fortyFifteen));
  });
  test(
    "Given player: 15 | other : 15 when player wins then score is 30/15", () => {
    let fifteenFifteen = {playerOne: Fifteen, playerTwo: Fifteen};
    let thirtyFifteen = {playerOne: Thirty, playerTwo: Fifteen};
    expect(scoreWhenPoints(fifteenFifteen, PlayerOne))
    |> toEqual(Points(thirtyFifteen));
  });
  test("Given player: 0 | other : 15 when other wins then score is 0/30", () => {
    let loveFifteen = {playerOne: Love, playerTwo: Fifteen};
    let loveThirty = {playerOne: Love, playerTwo: Thirty};
    expect(scoreWhenPoints(loveFifteen, other(PlayerOne)))
    |> toEqual(Points(loveThirty));
  });
  test(
    "Given player: 30 | other : 15 when player wins then score is 40/15", () => {
    let thirtyFifteen = {playerOne: Thirty, playerTwo: Fifteen};
    let fortyFifteen = {player: PlayerOne, otherPlayerPoint: Fifteen};
    expect(scoreWhenPoints(thirtyFifteen, PlayerOne))
    |> toEqual(Forty(fortyFifteen));
  });
});