import "reflect-metadata";
import {Response, Request, NextFunction} from 'express';
import { Container } from 'typedi';
import config from '../../config';
import { Result }  from '../../src/core/logic/Result';
import * as sinon from 'sinon';
import TruckController from '../../src/controllers/TruckController';
import ITruckService from '../../src/services/IServices/ITruckService';
import { ITruckDTO } from '../../src/dto/ITruckDTO';
import { describe } from 'node:test';
import 'mocha';


describe('TruckController', () => {
    const sandbox = sinon.createSandbox();
    beforeEach(() => {
        Container.reset();

        
        let truckSchemaInstance = require('../../src/persistence/schemas/truckSchema').default;
        Container.set("truckSchema", truckSchemaInstance);

        let truckRepoClass = require('../../src/repos/truckRepo').default;
        let truckRepoInstance = Container.get(truckRepoClass);
        Container.set("TruckRepo", truckRepoInstance);

        let truckServiceClass = require('../../src/services/TruckService').default;
        let truckServiceInstance = Container.get(truckServiceClass);
        Container.set("TruckService", truckServiceInstance);

    });
    
    afterEach(() => {
        sinon.restore();
        sandbox.restore();
    });

    it('createTruck', async () => {
        
        // Arrange
        let body = {
            truckID: "truckID",
            tare: 1,
            capacity: 1,
            maxBatteryCapacity: 1,
            autonomy: 1,
            fastChargeTime: 1
        };

        let req: Partial<Request> = {};
        req.body = body;

        let res: Partial<Response> = {
            json: sinon.spy(),
        };

        let next: Partial<NextFunction> = () => {};

    
        let truckServiceInstance = Container.get("TruckService");
  
        const mock = sinon.stub(truckServiceInstance, 'createTruck').returns(Promise.resolve(Result.ok<ITruckDTO>(body as ITruckDTO)));

        const truckController = new TruckController(truckServiceInstance as ITruckService);

        // Act
        await truckController.createTruck(<Request>req, <Response>res, <NextFunction>next);

        //Assert
        sinon.assert.calledOnce(mock);
        sinon.assert.calledWith(mock, sinon.match({
            truckID: "truckID",
            tare: 1,
            capacity: 1,
            maxBatteryCapacity: 1,
            autonomy: 1,
            fastChargeTime: 1
        }));
        
    });



    

});
