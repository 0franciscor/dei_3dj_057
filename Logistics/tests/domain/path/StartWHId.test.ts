import { expect } from "chai";
import {StartWHId} from "../../../src/domain/path/StartWHId"
describe ("Create DestinationWHID",()=>{
    it("warehouseID should be 1", async()=>{
        const startWHId = StartWHId.create("1");
        expect(startWHId.getValue().startWHId).to.equal("1");
    });
});

describe ("Create an invalid DestinationWHId", ()=>{
    it("warehouseID should return error", async()=> {
        const warehouseID= StartWHId.create("");
        expect (warehouseID.error).to.equal("Id must be provided");
    });
});