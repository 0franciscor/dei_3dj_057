import {Path} from '../../../src/domain/path/Path';
import { expect } from 'chai';
import "mocha";
import { DestinationWHId } from '../../../src/domain/path/DestinationWHId';

describe ("Create Path",()=>{
    it("path should be created", async () => {

        const path = Path.create({
            id: '',
            pathID : 'pathIDtest',
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        });
        expect(path.getValue()).to.equal(path.getValue());
        expect(path.getValue().pathID.id).to.equal('pathIDtest');
        expect(path.getValue().startWHId.startWHId).to.equal('WH5');
        expect(path.getValue().destinationWHId.destinationWHId).to.equal('WH6');
        expect(path.getValue().pathDistance.pathDistance).to.equal(200);
        expect(path.getValue().pathTravelTime.pathTravelTime).to.equal(20);
        expect(path.getValue().extraTravelTime.extraTravelTime).to.equal(0);
        expect(path.getValue().wastedEnergy.wastedEnergy).to.equal(30);
    });

    
})

describe ("Create invalid Path",()=>{
    it("path should return invalid pathID", async () => {

        const path = Path.create({
            id: '',
            pathID : '',
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        });
        expect(path.isFailure).to.equal(true)
    });

    it("path should return invalid startWhId", async () => {

        const path = Path.create({
            id: '',
            pathID : 'pathteste',
            startWHId : '',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        });
        expect(path.isFailure).to.equal(true)
    });

    it("path should return invalid destinationWHId", async () => {

        const path = Path.create({
            id: '',
            pathID : 'pathteste',
            startWHId : 'WH5',
            destinationWHId: '',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        });
        expect(path.isFailure).to.equal(true)
    });
   
    it("path should return invalid pathdistance when <0", async () => {

        const path = Path.create({
            id: '',
            pathID : 'pathteste',
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: -1,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        });
        expect(path.isFailure).to.equal(true)
    });

    it("path should return invalid pathTravelTime when <=0", async () => {

        const path = Path.create({
            id: '',
            pathID : 'pathteste',
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 100,
            pathTravelTime: 0,
            extraTravelTime: 0,
            wastedEnergy: 30
        });
        expect(path.isFailure).to.equal(true)
    });

    it("path should return invalid extraTravelTime when <0", async () => {

        const path = Path.create({
            id: '',
            pathID : 'pathteste',
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 100,
            pathTravelTime: 100,
            extraTravelTime: -1,
            wastedEnergy: 30
        });
        expect(path.isFailure).to.equal(true)
    });

    it("path should return invalid wasted Energy when <=0", async () => {

        const path = Path.create({
            id: '',
            pathID : 'pathteste',
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 100,
            pathTravelTime: 100,
            extraTravelTime: 0,
            wastedEnergy: 0
        });
        expect(path.isFailure).to.equal(true)
    });
});