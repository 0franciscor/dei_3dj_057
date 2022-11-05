import "reflect-metadata";
import {Response, Request, NextFunction} from 'express';
import { Container } from 'typedi';
import config from '../../config';
import { Result }  from '../../src/core/logic/Result';
import * as sinon from 'sinon';
import TruckController from '../../src/controllers/TruckController';
import ITruckService from '../../src/services/IServices/ITruckService';
import { ITruckDTO } from '../../src/dto/ITruckDTO';
import 'mocha';
import {expect} from "chai";

describe('TruckController Unit Tests', () => {
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

    it('createTruck returns truck JSON', async () => {
        
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
    
    it('createTruck returns 409 when "Truck Already Exists"', async () => {
        
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
            status: sinon.spy()
        };

        let next: Partial<NextFunction> = () => {};

    
        let truckServiceInstance = Container.get("TruckService");
  
        sinon.stub(truckServiceInstance, 'createTruck').returns(Promise.resolve(Result.fail<ITruckDTO>("Truck already exists")));

        const truckController = new TruckController(truckServiceInstance as ITruckService);

        // Act
        await truckController.createTruck(<Request>req, <Response>res, <NextFunction>next);

        //Assert
        
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 409);


        
    });    

    it('getTruck returns truck JSON', async () => {

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
        req.body = {
            truckID: "truckID"
        }

        let res: Partial<Response> = {
            json: sinon.spy(),
            status: sinon.spy()
        };

        let next: Partial<NextFunction> = () => {};

    
        let truckServiceInstance = Container.get("TruckService");
  
        sinon.stub(truckServiceInstance, 'getTruck').returns(Promise.resolve(Result.ok<ITruckDTO>(body as ITruckDTO)));

        const truckController = new TruckController(truckServiceInstance as ITruckService);

        // Act
        await truckController.getTruck(<Request>req, <Response>res, <NextFunction>next);
        
        //Assert
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, sinon.match(body));
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 200);

    });   

    it('getTruck returns 200', async () => {

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
        req.body = {
            truckID: "truckID"
        }

        let res: Partial<Response> = {
            status: sinon.spy()
        };

        let next: Partial<NextFunction> = () => {};

    
        let truckServiceInstance = Container.get("TruckService");
  
        sinon.stub(truckServiceInstance, 'getTruck').returns(Promise.resolve(Result.ok<ITruckDTO>(body as ITruckDTO)));

        const truckController = new TruckController(truckServiceInstance as ITruckService);

        // Act
        await truckController.getTruck(<Request>req, <Response>res, <NextFunction>next);
        
        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 200);

    });   



    it('getTruck returns "Trucks not found"', async () => {

        // Arrange

        let req: Partial<Request> = {};
        req.body = {
            truckID: "truckID"
        }

        let res: Partial<Response> = {
            status: sinon.spy(),
            send: sinon.spy()
        };

        let next: Partial<NextFunction> = () => {};

    
        let truckServiceInstance = Container.get("TruckService");
  

        sinon.stub(truckServiceInstance, 'getTruck').returns(Promise.resolve(Result.fail<ITruckDTO>("Truck not found")));

        const truckController = new TruckController(truckServiceInstance as ITruckService);

        // Act
        await truckController.getTruck(<Request>req, <Response>res, <NextFunction>next);
        
        //Assert

        sinon.assert.calledOnce(res.send);
        sinon.assert.calledWith(res.send, sinon.match("Truck not found"));
    });

    it('getTruck returns 404', async () => {

        // Arrange
        
        let req: Partial<Request> = {};
        req.body = {
            truckID: "truckID"
        }
        let res: Partial<Response> = {
            status: sinon.spy()
        };

        let next: Partial<NextFunction> = () => {};

    
        let truckServiceInstance = Container.get("TruckService");
  

        sinon.stub(truckServiceInstance, 'getTruck').returns(Promise.resolve(Result.fail<ITruckDTO>("Truck not found")));

        const truckController = new TruckController(truckServiceInstance as ITruckService);

        // Act
        await truckController.getTruck(<Request>req, <Response>res, <NextFunction>next);
        
        //Assert

        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 404);
    });

    it('getAllTrucks returns trucks', async () => {

        // Arrange
        let body = [{
            truckID: "truckID",
            tare: 1,
            capacity: 1,
            maxBatteryCapacity: 1,
            autonomy: 1,
            fastChargeTime: 1
        },
        {
            truckID: "truckID2",
            tare: 1,
            capacity: 1,
            maxBatteryCapacity: 1,
            autonomy: 1,
            fastChargeTime: 1
        }];
        let req: Partial<Request> = {};
        

        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };

        let next: Partial<NextFunction> = () => {};

    
        let truckServiceInstance = Container.get("TruckService");
  

        sinon.stub(truckServiceInstance, 'getAllTrucks').returns(Promise.resolve(Result.ok<ITruckDTO[]>(body as ITruckDTO[])));

        const truckController = new TruckController(truckServiceInstance as ITruckService);

        // Act
        await truckController.getAllTrucks(<Request>req, <Response>res, <NextFunction>next);
        
        //Assert
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, body);

        
    });

    it('getAllTrucks returns 200', async () => {

        // Arrange
        let body = [{
            truckID: "truckID",
            tare: 1,
            capacity: 1,
            maxBatteryCapacity: 1,
            autonomy: 1,
            fastChargeTime: 1
        },
        {
            truckID: "truckID2",
            tare: 1,
            capacity: 1,
            maxBatteryCapacity: 1,
            autonomy: 1,
            fastChargeTime: 1
        }];
        let req: Partial<Request> = {};
        

        let res: Partial<Response> = {
            status: sinon.spy()
        };

        let next: Partial<NextFunction> = () => {};

    
        let truckServiceInstance = Container.get("TruckService");
  


        sinon.stub(truckServiceInstance, 'getAllTrucks').returns(Promise.resolve(Result.ok<ITruckDTO[]>(body as ITruckDTO[])));

        const truckController = new TruckController(truckServiceInstance as ITruckService);

        // Act
        await truckController.getAllTrucks(<Request>req, <Response>res, <NextFunction>next);
        
        //Assert

        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 200);

    });



    it('deleteTruck returns truck', async () => {

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
            req.body= {
                truckID: "truckID"
            }

        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };

        let next: Partial<NextFunction> = () => {};

    
        let truckServiceInstance = Container.get("TruckService");
  

        sinon.stub(truckServiceInstance, 'deleteTruck').returns(Promise.resolve(Result.ok<ITruckDTO>(body as ITruckDTO)));

        const truckController = new TruckController(truckServiceInstance as ITruckService);

        // Act
        await truckController.deleteTruck(<Request>req, <Response>res, <NextFunction>next);
        
        //Assert
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, body);

        
    });

    it('deleteTruck returns 200', async () => {

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
        req.body= {
            truckID: "truckID"
        }

        let res: Partial<Response> = {
            status: sinon.spy()
        };

        let next: Partial<NextFunction> = () => {};

    
        let truckServiceInstance = Container.get("TruckService");
  


        sinon.stub(truckServiceInstance, 'deleteTruck').returns(Promise.resolve(Result.ok<ITruckDTO>(body as ITruckDTO)));

        const truckController = new TruckController(truckServiceInstance as ITruckService);

        // Act
        await truckController.deleteTruck(<Request>req, <Response>res, <NextFunction>next);
        
        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 200);

    });



    it('deleteTruck returns "Truck not found"', async () => {

        // Arrange
        let req: Partial<Request> = {};
            req.body= {
                truckID: "truckID"
            }

        let res: Partial<Response> = {
            status: sinon.spy(),
            send: sinon.spy()
        };

        let next: Partial<NextFunction> = () => {};

    
        let truckServiceInstance = Container.get("TruckService");
  

        sinon.stub(truckServiceInstance, 'deleteTruck').returns(Promise.resolve(Result.fail<ITruckDTO>("Truck not found")));

        const truckController = new TruckController(truckServiceInstance as ITruckService);

        // Act
        await truckController.deleteTruck(<Request>req, <Response>res, <NextFunction>next);
        
        //Assert
        sinon.assert.calledOnce(res.send);
        sinon.assert.calledWith(res.send, "Truck not found");

        
    });

    it('deleteTruck returns 404', async () => {

        // Arrange
        let req: Partial<Request> = {};
            req.body= {
                truckID: "truckID"
            }

        let res: Partial<Response> = {
            status: sinon.spy()
        };

        let next: Partial<NextFunction> = () => {};

    
        let truckServiceInstance = Container.get("TruckService");
  

        sinon.stub(truckServiceInstance, 'deleteTruck').returns(Promise.resolve(Result.fail<ITruckDTO>("Truck not found")));

        const truckController = new TruckController(truckServiceInstance as ITruckService);

        // Act
        await truckController.deleteTruck(<Request>req, <Response>res, <NextFunction>next);
        
        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 404);

    });





    it('update returns truck', async () => {

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
        req.body=body;

        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };

        let next: Partial<NextFunction> = () => {};

    
        let truckServiceInstance = Container.get("TruckService");
  

        sinon.stub(truckServiceInstance, 'updateTruck').returns(Promise.resolve(Result.ok<ITruckDTO>(body as ITruckDTO)));

        const truckController = new TruckController(truckServiceInstance as ITruckService);

        // Act
        await truckController.updateTruck(<Request>req, <Response>res, <NextFunction>next);
        
        //Assert
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, body);

        
    });

    it('updateTruck returns 200', async () => {

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
        req.body=body;

        let res: Partial<Response> = {
            status: sinon.spy()
        };

        let next: Partial<NextFunction> = () => {};

    
        let truckServiceInstance = Container.get("TruckService");
  

        sinon.stub(truckServiceInstance, 'updateTruck').returns(Promise.resolve(Result.ok<ITruckDTO>(body as ITruckDTO)));

        const truckController = new TruckController(truckServiceInstance as ITruckService);

        // Act
        await truckController.updateTruck(<Request>req, <Response>res, <NextFunction>next);
        
        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 200);

    });



    it('updateTruck returns "Truck not found"', async () => {

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
        req.body=body;

        let res: Partial<Response> = {
            status: sinon.spy(),
            send: sinon.spy()
        };

        let next: Partial<NextFunction> = () => {};

    
        let truckServiceInstance = Container.get("TruckService");
  

        sinon.stub(truckServiceInstance, 'updateTruck').returns(Promise.resolve(Result.fail<ITruckDTO>("Truck not found")));

        const truckController = new TruckController(truckServiceInstance as ITruckService);

        // Act
        await truckController.updateTruck(<Request>req, <Response>res, <NextFunction>next);
        
        //Assert
        sinon.assert.calledOnce(res.send);
        sinon.assert.calledWith(res.send, "Truck not found");

        
    });

    it('deleteTruck returns 404', async () => {

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
        req.body=body;

        let res: Partial<Response> = {
            status: sinon.spy()
        };

        let next: Partial<NextFunction> = () => {};

    
        let truckServiceInstance = Container.get("TruckService");
  

        sinon.stub(truckServiceInstance, 'updateTruck').returns(Promise.resolve(Result.fail<ITruckDTO>("Truck not found")));

        const truckController = new TruckController(truckServiceInstance as ITruckService);

        // Act
        await truckController.updateTruck(<Request>req, <Response>res, <NextFunction>next);
        
        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 404);

    });







});
