import "reflect-metadata";
import {Response, Request, NextFunction} from 'express';
import { Container } from 'typedi';
import { Result }  from '../../src/core/logic/Result';
import * as sinon from 'sinon';
import WarehouseController from '../../src/controllers/warehouseController';
import 'mocha';

describe('WarehouseController Unit Tests', () => {
    const sandbox = sinon.createSandbox();
    beforeEach(() => {
        Container.reset();
    });
    
    afterEach(() => {
        sinon.restore();
        sandbox.restore();
    });


    it('createWarehouse returns warehouse JSON', async()=>{

        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {};
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const warehouseController = new WarehouseController();
    
        sinon.stub(warehouseController,'fetch').returns({
            status: 201,
            json: () => {
                return {
                    "message": "Error creating warehouse"
                }
            }
        });
    
        //Act
        await warehouseController.createWarehouse(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, sinon.match.has('message', 'Error creating warehouse'));    
    });

    it('createWarehouse returns status', async()=>{

        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {};
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const warehouseController = new WarehouseController();
    
        sinon.stub(warehouseController,'fetch').returns({
            status: 201,
            json: () => {
                return {
                    "message": "Warehouse created"
                }
            }
        });
    
        //Act
        await warehouseController.createWarehouse(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 201);    
    });


    it('createWarehouse error returns error', async()=>{

        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {};
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const warehouseController = new WarehouseController();
    
        sinon.stub(warehouseController,'fetch').returns({
            status: 409,
            json: () => {
                return {
                    "message": "Error creating warehouse"
                }
            }
        });
    
        //Act
        await warehouseController.createWarehouse(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, sinon.match.has('message', 'Error creating warehouse'));    
    });

    it('createWarehouse error returns status', async()=>{

        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {};
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const warehouseController = new WarehouseController();
    
        sinon.stub(warehouseController,'fetch').returns({
            status: 409,
            json: () => {
                return {
                    "message": "Error creating warehouse"
                }
            }
        });
    
        //Act
        await warehouseController.createWarehouse(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 409);    
    });


    it('createWarehouseProlog returns warehouse JSON', async()=>{

        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {};
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const warehouseController = new WarehouseController();
    
        sinon.stub(warehouseController,'fetch').returns({
            status: 201,
            json: () => {
                return {
                    "message": "Warehouse created"
                }
            }
        });
    
        //Act
        await warehouseController.createWarehouseProlog(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, sinon.match.has('message', 'Warehouse created'));    
    });

    it('createWarehouseProlog returns status', async()=>{

        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {};
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const warehouseController = new WarehouseController();
    
        sinon.stub(warehouseController,'fetch').returns({
            status: 201,
            json: () => {
                return {
                    "message": "Warehouse created"
                }
            }
        });
    
        //Act
        await warehouseController.createWarehouseProlog(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 201);    
    });


    it('createWarehouseProlog error returns error', async()=>{

        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {};
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const warehouseController = new WarehouseController();
    
        sinon.stub(warehouseController,'fetch').returns({
            status: 409,
            json: () => {
                return {
                    "message": "Error creating warehouse"
                }
            }
        });
    
        //Act
        await warehouseController.createWarehouseProlog(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, sinon.match.has('message', 'Error creating warehouse'));    
    });

    it('createWarehouseProlog error returns status', async()=>{

        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {};
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const warehouseController = new WarehouseController();
    
        sinon.stub(warehouseController,'fetch').returns({
            status: 201,
            json: () => {
                return {
                    "message": "Error creating warehouse"
                }
            }
        });
    
        //Act
        await warehouseController.createWarehouseProlog(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 201);    
    });


    it('getAllWarehouses returns JSON', async()=>{
        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {};
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const warehouseController = new WarehouseController();
    
        sinon.stub(warehouseController,'fetch').returns({
            status: 200,
            json: () => {
                return {
                    "message": "Warehouses found"
                }
            }
        });
    
        //Act
        await warehouseController.getAllWarehouse(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, sinon.match.has('message', 'Warehouses found'));    
    });

    it('getAllWarehouses returns status', async()=>{
        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {};
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const warehouseController = new WarehouseController();
    
        sinon.stub(warehouseController,'fetch').returns({
            status: 200,
            json: () => {
                return {
                    "message": "Warehouses found"
                }
            }
        });
    
        //Act
        await warehouseController.getAllWarehouse(<Request>req,<Response>res,<NextFunction>next);
        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 200);    
    });


    it('getAllWarehouses error returns JSON', async()=>{
        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {};
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const warehouseController = new WarehouseController();
    
        sinon.stub(warehouseController,'fetch').returns({
            status: 404,
            json: () => {
                return {
                    "message": "Error Getting Warehouses"
                }
            }
        });
    
        //Act
        await warehouseController.getAllWarehouse(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, sinon.match.has('message', 'Error Getting Warehouses'));    
    });

    it('getAllWarehouses error returns status', async()=>{
        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {};
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const warehouseController = new WarehouseController();
    
        sinon.stub(warehouseController,'fetch').returns({
            status: 404,
            json: () => {
                return {
                    "message": "Error getting all warehouses"
                }
            }
        });
    
        //Act
        await warehouseController.getAllWarehouse(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 404);    
    });




    it('getWarehouses error returns status', async()=>{
        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {};
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const warehouseController = new WarehouseController();
    
        sinon.stub(warehouseController,'fetch').returns({
            status: 404,
            json: () => {
                return {
                    "message": "Error getting warehouses"
                }
            }
        });
    
        //Act
        await warehouseController.getAllWarehouse(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 404);    
    });

    it('editWarehouse returns JSON', async()=>{
        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {};
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const warehouseController = new WarehouseController();
    
        sinon.stub(warehouseController,'fetch').returns({
            status: 200,
            json: () => {
                return {
                    "message": "Warehouse updated"
                }
            }
        });
    
        //Act
        await warehouseController.editWarehouse(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, sinon.match.has('message', 'Warehouse updated'));    
    });

    it('editWarehouse returns status', async()=>{
        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {};
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const warehouseController = new WarehouseController();
    
        sinon.stub(warehouseController,'fetch').returns({
            status: 200,
            json: () => {
                return {
                    "message": "Warehouse updated"
                }
            }
        });
    
        //Act
        await warehouseController.editWarehouse(<Request>req,<Response>res,<NextFunction>next);
    
    
        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 200);    
    });

    it('editWarehouse error returns JSON', async()=>{
        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {};
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const warehouseController = new WarehouseController();
    
        sinon.stub(warehouseController,'fetch').returns({
            status: 200,
            json: () => {
                return {
                    "message": "Error editing warehouse"
                }
            }
        });
    
        //Act
        await warehouseController.editWarehouse(<Request>req,<Response>res,<NextFunction>next);
    
    
        //Assert
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, sinon.match.has('message', 'Error editing warehouse'));    
    });

    it('editWarehouse error returns status', async()=>{
        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {};
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const warehouseController = new WarehouseController();
    
        sinon.stub(warehouseController,'fetch').returns({
            status: 404,
            json: () => {
                return {
                    "message": "Error editing warehouse"
                }
            }
        });
    
        //Act
        await warehouseController.editWarehouse(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 404);    
    });


    it('editWarehouseProlog returns JSON', async()=>{
        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {};
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const warehouseController = new WarehouseController();
    
        sinon.stub(warehouseController,'fetch').returns({
            status: 200,
            json: () => {
                return {
                    "message": "Warehouse updated"
                }
            }
        });
    
        //Act
        await warehouseController.editWarehouseProlog(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, sinon.match.has('message', 'Warehouse updated'));    
    });

    it('editWarehouseProlog returns status', async()=>{
        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {};
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const warehouseController = new WarehouseController();
    
        sinon.stub(warehouseController,'fetch').returns({
            status: 200,
            json: () => {
                return {
                    "message": "Warehouse updated"
                }
            }
        });
    
        //Act
        await warehouseController.editWarehouseProlog(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 200);    
    });

    it('editWarehouseProlog error returns JSON', async()=>{
        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {};
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const warehouseController = new WarehouseController();
    
        sinon.stub(warehouseController,'fetch').returns({
            status: 404,
            json: () => {
                return {
                    "message": "Error updating warehouse"
                }
            }
        });
    
        //Act
        await warehouseController.editWarehouseProlog(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, sinon.match.has('message', 'Error updating warehouse'));    
    });

    it('editWarehouseProlog error returns status', async()=>{
        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {};
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const warehouseController = new WarehouseController();
    
        sinon.stub(warehouseController,'fetch').returns({
            status: 404,
            json: () => {
                return {
                    "message": "Error editing warehouse"
                }
            }
        });
    
        //Act
        await warehouseController.editWarehouseProlog(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 404);    
    });


});