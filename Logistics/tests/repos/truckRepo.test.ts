import "reflect-metadata";
import { Container } from 'typedi';
import * as sinon from 'sinon';
import TruckRepo from "../../src/repos/TruckRepo";
import { ITruckDTO } from '../../src/dto/ITruckDTO';
import 'mocha';
import {expect} from "chai";
import { TruckMap } from "../../src/mappers/TruckMap";
import { Truck } from "../../src/domain/truck/Truck";
import { ITruckPersistence } from "../../src/dataschema/ITruckPersistence";
import mongoose from "mongoose";
import { execPath } from "node:process";

describe('TruckRepo Unit Tests', () => {

    const sandbox = sinon.createSandbox();
    beforeEach(() => {
        Container.reset();

        let truckSchemaInstance = require('../../src/persistence/schemas/truckSchema').default;
        Container.set("truckSchema", truckSchemaInstance);

    });
    
    afterEach(() => {
        sinon.restore();
        sandbox.restore();
    });

    it('Exists should return true', async () => {

    
        const truckDTO = {
            truckID: "123",
            autonomy: 100,
            capacity: 100,
            fastChargeTime: 100,
            maxBatteryCapacity: 100,
            tare: 100
        } as ITruckDTO;

        const truckSchemaInstance = Container.get("truckSchema");

        sinon.stub(truckSchemaInstance, "findById").returns(true);
        const truckRepo = new TruckRepo(truckSchemaInstance as any);
        const answer = await truckRepo.exists(TruckMap.toDomain(truckDTO));
        expect(answer).to.be.true;
    });

    it('Save should return truck', async () => {

    
        const truckDTO = {
            domainId: "123",
            truckID: "123",
            autonomy: 100,
            capacity: 100,
            fastChargeTime: 100,
            maxBatteryCapacity: 100,
            tare: 100
        } as ITruckPersistence;

        const truckSchemaInstance = Container.get("truckSchema");
        const truck = TruckMap.toDomain(truckDTO);
        sinon.stub(truckSchemaInstance, "findOne").returns(null);
        sinon.stub(truckSchemaInstance, "create").returns(truckDTO as ITruckPersistence);
        const truckRepo = new TruckRepo(truckSchemaInstance as any);
        const answer = await truckRepo.save(truck);
        expect(answer.truckID.id).to.equal(truck.truckID.id);
        expect(answer.tare.tare).to.equal(truck.tare.tare);
        expect(answer.capacity.capacity).to.equal(truck.capacity.capacity);
        expect(answer.maxBatteryCapacity.capacity).to.equal(truck.maxBatteryCapacity.capacity);
        expect(answer.autonomy.autonomy).to.equal(truck.autonomy.autonomy);
        expect(answer.fastChargeTime.time).to.equal(truck.fastChargeTime.time);

    });

    

});
