import { expect } from "chai";
import {DestinationWHId} from "../../../src/domain/path/DestinationWHId"
describe ("Create DestinationWHID",()=>{
    it("warehouseID should be 1", async()=>{
        const destinationWHID = DestinationWHId.create("1");
        expect(destinationWHID.getValue().destinationWHId).to.equal("1");
    });
});

describe ("Create an invalid DestinationWHId", ()=>{
    it("warehouseID should return error", async()=> {
        const warehouseID= DestinationWHId.create("");
        expect (warehouseID.error).to.equal("Id must be provided");
    })
})