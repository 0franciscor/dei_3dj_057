import { ExtraTravelTime } from "../../../src/domain/path/ExtraTravelTime";
import { expect } from "chai";
import "mocha";

describe("Create path extra travel time",()=>{
    it("ExtraTravelTime should be 1", async ()=> {
        const extraTravelTime = ExtraTravelTime.create(1);
        expect(extraTravelTime.getValue().extraTravelTime).to.equal(1);
    });
});

describe ("Create invalid extra Travel time", ()=> {
    it("ExtraTravelTime should return error", async () => {
        const extraTravelTime = ExtraTravelTime.create(-1);
        expect(extraTravelTime.error).to.equal("This extra travel time indicator is not acceptable");
    });

});
