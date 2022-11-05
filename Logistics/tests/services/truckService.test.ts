import "reflect-metadata";
import {Response, Request, NextFunction} from 'express';
import { Container } from 'typedi';
import config from '../../config';
import { Result }  from '../../src/core/logic/Result';
import * as sinon from 'sinon';
import TruckService from '../../src/services/TruckService';
import ITruckRepo from "../../src/repos/IRepos/ITruckRepo";
import { ITruckDTO } from '../../src/dto/ITruckDTO';
import 'mocha';
import {expect} from "chai";
import exp from "node:constants";

describe('TruckService Unit Tests', () => {

    const sandbox = sinon.createSandbox();
    beforeEach(() => {
        Container.reset();

        
        let truckSchemaInstance = require('../../src/persistence/schemas/truckSchema').default;
        Container.set("truckSchema", truckSchemaInstance);

        let truckRepoClass = require('../../src/repos/truckRepo').default;
        let truckRepoInstance = Container.get(truckRepoClass);
        Container.set("TruckRepo", truckRepoInstance);

    });
    
    afterEach(() => {
        sinon.restore();
        sandbox.restore();
    });

    it('exists return true', async () => {
        
        let body = {
            truckID: "truckID",
            tare: 1,
            capacity: 1,
            maxBatteryCapacity: 1,
            autonomy: 1,
            fastChargeTime: 1
        };

        let truckRepoInstance = Container.get("TruckRepo");

        sinon.stub(truckRepoInstance, "getTruckById").returns(Promise.resolve(body));
        const truckService = new TruckService(truckRepoInstance as ITruckRepo);
        const answer = await truckService.exist(body.truckID);
        expect(answer.getValue()).to.equal(true);

    });

    it('exists return false', async () => {
        
        let body = {
            truckID: "truckID",
            tare: 1,
            capacity: 1,
            maxBatteryCapacity: 1,
            autonomy: 1,
            fastChargeTime: 1
        };

        let truckRepoInstance = Container.get("TruckRepo");

        sinon.stub(truckRepoInstance, "getTruckById").returns(Promise.resolve(null));
        const truckService = new TruckService(truckRepoInstance as ITruckRepo);
        const answer = await truckService.exist(body.truckID);
        expect(answer.getValue()).to.equal(false);

    });



    it('createTruck returns truck + success', async () => {
        
        let body = {
            truckID: "truckID",
            tare: 1,
            capacity: 1,
            maxBatteryCapacity: 1,
            autonomy: 1,
            fastChargeTime: 1
        };

        let truckRepoInstance = Container.get("TruckRepo");

        sinon.stub(truckRepoInstance, "getTruckById").returns(Promise.resolve(null));
        sinon.stub(truckRepoInstance, "save").returns(Promise.resolve(body));
        const truckService = new TruckService(truckRepoInstance as ITruckRepo);
        const answer = await truckService.createTruck(body as ITruckDTO);
        answer.getValue().id = "id";
        expect(answer.getValue().truckID).to.equal(body.truckID);
        expect(answer.getValue().tare).to.equal(body.tare);
        expect(answer.getValue().capacity).to.equal(body.capacity);
        expect(answer.getValue().maxBatteryCapacity).to.equal(body.maxBatteryCapacity);
        expect(answer.getValue().autonomy).to.equal(body.autonomy);
        expect(answer.getValue().fastChargeTime).to.equal(body.fastChargeTime);

            
        
    });

    it('createTruck returns "truck already exists"', async () => {
        
        let body = {
            truckID: "truckID",
            tare: 1,
            capacity: 1,
            maxBatteryCapacity: 1,
            autonomy: 1,
            fastChargeTime: 1
        };

        let truckRepoInstance = Container.get("TruckRepo");

        sinon.stub(truckRepoInstance, "getTruckById").returns(Promise.resolve(body));
        const truckService = new TruckService(truckRepoInstance as ITruckRepo);
        const answer = await truckService.createTruck(body as ITruckDTO);
        console.log(answer);
        expect(answer.errorValue()).to.equal("Truck already exists");

    });



    it('getTruck returns truck + success', async () => {
        
        let body = {
            id: "id",
            truckID: "truckID",
            tare: 1,
            capacity: 1,
            maxBatteryCapacity: 1,
            autonomy: 1,
            fastChargeTime: 1
        };

        let truckRepoInstance = Container.get("TruckRepo");

        sinon.stub(truckRepoInstance, "getTruckById").returns(body);
        const truckService = new TruckService(truckRepoInstance as ITruckRepo);
        const answer = await truckService.getTruck(body.truckID);
        console.log(answer);
        expect(answer.getValue().truckID).to.equal(body.truckID);
        expect(answer.getValue().tare).to.equal(body.tare);
        expect(answer.getValue().capacity).to.equal(body.capacity);
        expect(answer.getValue().maxBatteryCapacity).to.equal(body.maxBatteryCapacity);
        expect(answer.getValue().autonomy).to.equal(body.autonomy);
        expect(answer.getValue().fastChargeTime).to.equal(body.fastChargeTime);

            
        
    });

    it('getTruck returns "truck not found"', async () => {
        
        let body = {
            truckID: "truckID",
            tare: 1,
            capacity: 1,
            maxBatteryCapacity: 1,
            autonomy: 1,
            fastChargeTime: 1
        };

        let truckRepoInstance = Container.get("TruckRepo");

        sinon.stub(truckRepoInstance, "getTruckById").returns(null);
        const truckService = new TruckService(truckRepoInstance as ITruckRepo);
        const answer = await truckService.getTruck(body.truckID);

        expect(answer.errorValue()).to.equal("Truck not found");

    });

});