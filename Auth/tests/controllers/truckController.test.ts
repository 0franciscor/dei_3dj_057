import { NextFunction, Request, Response } from 'express';
import 'mocha';
import "reflect-metadata";
import * as sinon from 'sinon';
import { Container } from 'typedi';
import TruckController from '../../src/controllers/truckController';


describe('TruckController Unit Tests', () => {
    const sandbox = sinon.createSandbox();
    beforeEach(() => {
        Container.reset();
    });
    
    afterEach(() => {
        sinon.restore();
        sandbox.restore();
    });


    it('createTruck returns truck JSON', async()=>{

        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {
            cookies:{
                jwt: 'token'
            },
            headers: {
                authorization: 'Bearer token',
                host: 'azure'                
            },
            get: function(header: string) {
                return this.headers[header];
            }
        };
        
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};
        
        const truckController = new TruckController();
        sinon.stub(req,'get').withArgs('host').returns(false)
        sinon.stub(truckController, 'isAuthenticated').returns(true);
        sinon.stub(truckController, 'isAuthorized').returns(true);
        

        sinon.stub(truckController,'fetch').returns({
            status: 201,
            json: () => {
                return {
                    "message": "Truck created"
                }
            }
        });
    
        //Act
        await truckController.createTruck(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, sinon.match.has('message', 'Truck created'));    
    });

    it('createTruck returns status', async()=>{

        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {
            cookies:{
                jwt: 'token'
            },
            headers: {
                authorization: 'Bearer token',
                host: 'azure'                
            },
            get: function(header: string) {
                return this.headers[header];
            }
        };
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const truckController = new TruckController();
        sinon.stub(req,'get').withArgs('host').returns(false)
        sinon.stub(truckController, 'isAuthenticated').returns(true);
        sinon.stub(truckController, 'isAuthorized').returns(true);
    
        sinon.stub(truckController,'fetch').returns({
            status: 201,
            json: () => {
                return {
                    "message": "Truck created"
                }
            }
        });
    
        //Act
        await truckController.createTruck(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 201);    
    });


    it('createTruck error returns error', async()=>{

        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {
            cookies:{
                jwt: 'token'
            },
            headers: {
                authorization: 'Bearer token',
                host: 'azure'                
            },
            get: function(header: string) {
                return this.headers[header];
            }
        };
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const truckController = new TruckController();
        sinon.stub(req,'get').withArgs('host').returns(false)
        sinon.stub(truckController, 'isAuthenticated').returns(true);
        sinon.stub(truckController, 'isAuthorized').returns(true);
    
        sinon.stub(truckController,'fetch').returns({
            status: 409,
            json: () => {
                return {
                    "message": "Error creating truck"
                }
            }
        });
    
        //Act
        await truckController.createTruck(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, sinon.match.has('message', 'Error creating truck'));    
    });

    it('createTruck error returns status', async()=>{

        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {
            cookies:{
                jwt: 'token'
            },
            headers: {
                authorization: 'Bearer token',
                host: 'azure'                
            },
            get: function(header: string) {
                return this.headers[header];
            }
        };
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const truckController = new TruckController();
        sinon.stub(req,'get').withArgs('host').returns(false)
        sinon.stub(truckController, 'isAuthenticated').returns(true);
        sinon.stub(truckController, 'isAuthorized').returns(true);
    
        sinon.stub(truckController,'fetch').returns({
            status: 409,
            json: () => {
                return {
                    "message": "Error creating truck"
                }
            }
        });
    
        //Act
        await truckController.createTruck(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 409);    
    });


    it('createTruckProlog returns truck JSON', async()=>{

        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {
            cookies:{
                jwt: 'token'
            },
            headers: {
                authorization: 'Bearer token',
                host: 'azure'                
            },
            get: function(header: string) {
                return this.headers[header];
            }
        };
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const truckController = new TruckController();
        sinon.stub(req,'get').withArgs('host').returns(false)
        sinon.stub(truckController, 'isAuthenticated').returns(true);
        sinon.stub(truckController, 'isAuthorized').returns(true);
    
        sinon.stub(truckController,'fetch').returns({
            status: 201,
            json: () => {
                return {
                    "message": "Truck created"
                }
            }
        });
    
        //Act
        await truckController.createTruckProlog(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, sinon.match.has('message', 'Truck created'));    
    });

    it('createTruckProlog returns status', async()=>{

        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {
            cookies:{
                jwt: 'token'
            },
            headers: {
                authorization: 'Bearer token',
                host: 'azure'                
            },
            get: function(header: string) {
                return this.headers[header];
            }
        };
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const truckController = new TruckController();
        sinon.stub(req,'get').withArgs('host').returns(false)
        sinon.stub(truckController, 'isAuthenticated').returns(true);
        sinon.stub(truckController, 'isAuthorized').returns(true);
    
        sinon.stub(truckController,'fetch').returns({
            status: 201,
            json: () => {
                return {
                    "message": "Truck created"
                }
            }
        });
    
        //Act
        await truckController.createTruckProlog(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 201);    
    });


    it('createTruckProlog error returns error', async()=>{

        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {
            cookies:{
                jwt: 'token'
            },
            headers: {
                authorization: 'Bearer token',
                host: 'azure'                
            },
            get: function(header: string) {
                return this.headers[header];
            }
        };
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const truckController = new TruckController();
        sinon.stub(req,'get').withArgs('host').returns(false)
        sinon.stub(truckController, 'isAuthenticated').returns(true);
        sinon.stub(truckController, 'isAuthorized').returns(true);
    
        sinon.stub(truckController,'fetch').returns({
            status: 409,
            json: () => {
                return {
                    "message": "Error creating truck"
                }
            }
        });
    
        //Act
        await truckController.createTruckProlog(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, sinon.match.has('message', 'Error creating truck'));    
    });

    it('createTruckProlog error returns status', async()=>{

        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {
            cookies:{
                jwt: 'token'
            },
            headers: {
                authorization: 'Bearer token',
                host: 'azure'                
            },
            get: function(header: string) {
                return this.headers[header];
            }
        };
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const truckController = new TruckController();
        sinon.stub(req,'get').withArgs('host').returns(false)
        sinon.stub(truckController, 'isAuthenticated').returns(true);
        sinon.stub(truckController, 'isAuthorized').returns(true);
    
        sinon.stub(truckController,'fetch').returns({
            status: 409,
            json: () => {
                return {
                    "message": "Error creating truck"
                }
            }
        });
    
        //Act
        await truckController.createTruckProlog(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 409);    
    });


    it('getAllTrucks returns JSON', async()=>{
        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {
            cookies:{
                jwt: 'token'
            },
            headers: {
                authorization: 'Bearer token',
                host: 'azure'                
            },
            get: function(header: string) {
                return this.headers[header];
            }
        };
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const truckController = new TruckController();
        sinon.stub(req,'get').withArgs('host').returns(false)
        sinon.stub(truckController, 'isAuthenticated').returns(true);
        sinon.stub(truckController, 'isAuthorized').returns(true);
    
        sinon.stub(truckController,'fetch').returns({
            status: 200,
            json: () => {
                return {
                    "message": "Trucks found"
                }
            }
        });
    
        //Act
        await truckController.getAllTruck(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, sinon.match.has('message', 'Trucks found'));    
    });

    it('getAllTrucks returns status', async()=>{
        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {
            cookies:{
                jwt: 'token'
            },
            headers: {
                authorization: 'Bearer token',
                host: 'azure'                
            },
            get: function(header: string) {
                return this.headers[header];
            }
        };
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const truckController = new TruckController();
        sinon.stub(req,'get').withArgs('host').returns(false)
        sinon.stub(truckController, 'isAuthenticated').returns(true);
        sinon.stub(truckController, 'isAuthorized').returns(true);
    
        sinon.stub(truckController,'fetch').returns({
            status: 200,
            json: () => {
                return {
                    "message": "Trucks found"
                }
            }
        });
    
        //Act
        await truckController.getAllTruck(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 200);    
    });


    it('getAllTrucks error returns JSON', async()=>{
        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {
            cookies:{
                jwt: 'token'
            },
            headers: {
                authorization: 'Bearer token',
                host: 'azure'                
            },
            get: function(header: string) {
                return this.headers[header];
            }
        };
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const truckController = new TruckController();
        sinon.stub(req,'get').withArgs('host').returns(false)
        sinon.stub(truckController, 'isAuthenticated').returns(true);
        sinon.stub(truckController, 'isAuthorized').returns(true);
    
        sinon.stub(truckController,'fetch').returns({
            status: 404,
            json: () => {
                return {
                    "message": "Error getting all trucks"
                }
            }
        });
    
        //Act
        await truckController.getAllTruck(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, sinon.match.has('message', 'Error getting all trucks'));    
    });

    it('getAllTrucks error returns status', async()=>{
        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {
            cookies:{
                jwt: 'token'
            },
            headers: {
                authorization: 'Bearer token',
                host: 'azure'                
            },
            get: function(header: string) {
                return this.headers[header];
            }
        };
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const truckController = new TruckController();
        sinon.stub(req,'get').withArgs('host').returns(false)
        sinon.stub(truckController, 'isAuthenticated').returns(true);
        sinon.stub(truckController, 'isAuthorized').returns(true);
    
        sinon.stub(truckController,'fetch').returns({
            status: 404,
            json: () => {
                return {
                    "message": "Error getting all trucks"
                }
            }
        });
    
        //Act
        await truckController.getAllTruck(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 404);    
    });


    it('getTruck returns JSON', async()=>{
        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {
            cookies:{
                jwt: 'token'
            },
            headers: {
                authorization: 'Bearer token',
                host: 'azure'                
            },
            get: function(header: string) {
                return this.headers[header];
            }
        };
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const truckController = new TruckController();
        sinon.stub(req,'get').withArgs('host').returns(false)
        sinon.stub(truckController, 'isAuthenticated').returns(true);
        sinon.stub(truckController, 'isAuthorized').returns(true);
    
        sinon.stub(truckController,'fetch').returns({
            status: 200,
            json: () => {
                return {
                    "message": "Truck found"
                }
            }
        });
    
        //Act
        await truckController.getTruck(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, sinon.match.has('message', 'Truck found'));    
    });

    it('getTruck returns status', async()=>{
        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {
            cookies:{
                jwt: 'token'
            },
            headers: {
                authorization: 'Bearer token',
                host: 'azure'                
            },
            get: function(header: string) {
                return this.headers[header];
            }
        };
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const truckController = new TruckController();
        sinon.stub(req,'get').withArgs('host').returns(false)
        sinon.stub(truckController, 'isAuthenticated').returns(true);
        sinon.stub(truckController, 'isAuthorized').returns(true);
    
        sinon.stub(truckController,'fetch').returns({
            status: 200,
            json: () => {
                return {
                    "message": "Truck found"
                }
            }
        });
    
        //Act
        await truckController.getTruck(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 200);    
    });


    it('getTruck error returns JSON', async()=>{
        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {
            cookies:{
                jwt: 'token'
            },
            headers: {
                authorization: 'Bearer token',
                host: 'azure'                
            },
            get: function(header: string) {
                return this.headers[header];
            }
        };
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const truckController = new TruckController();
        sinon.stub(req,'get').withArgs('host').returns(false)
        sinon.stub(truckController, 'isAuthenticated').returns(true);
        sinon.stub(truckController, 'isAuthorized').returns(true);
    
        sinon.stub(truckController,'fetch').returns({
            status: 404,
            json: () => {
                return {
                    "message": "Error getting truck"
                }
            }
        });
    
        //Act
        await truckController.getTruck(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, sinon.match.has('message', 'Error getting truck'));    
    });

    it('getTrucks error returns status', async()=>{
        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {
            cookies:{
                jwt: 'token'
            },
            headers: {
                authorization: 'Bearer token',
                host: 'azure'                
            },
            get: function(header: string) {
                return this.headers[header];
            }
        };
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const truckController = new TruckController();
        sinon.stub(req,'get').withArgs('host').returns(false)
        sinon.stub(truckController, 'isAuthenticated').returns(true);
        sinon.stub(truckController, 'isAuthorized').returns(true);
    
        sinon.stub(truckController,'fetch').returns({
            status: 404,
            json: () => {
                return {
                    "message": "Error getting trucks"
                }
            }
        });
    
        //Act
        await truckController.getAllTruck(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 404);    
    });

    it('editTruck returns JSON', async()=>{
        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {
            cookies:{
                jwt: 'token'
            },
            headers: {
                authorization: 'Bearer token',
                host: 'azure'                
            },
            get: function(header: string) {
                return this.headers[header];
            }
        };
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const truckController = new TruckController();
        sinon.stub(req,'get').withArgs('host').returns(false)
        sinon.stub(truckController, 'isAuthenticated').returns(true);
        sinon.stub(truckController, 'isAuthorized').returns(true);
    
        sinon.stub(truckController,'fetch').returns({
            status: 200,
            json: () => {
                return {
                    "message": "Truck updated"
                }
            }
        });
    
        //Act
        await truckController.editTruck(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, sinon.match.has('message', 'Truck updated'));    
    });

    it('editTruck returns status', async()=>{
        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {
            cookies:{
                jwt: 'token'
            },
            headers: {
                authorization: 'Bearer token',
                host: 'azure'                
            },
            get: function(header: string) {
                return this.headers[header];
            }
        };
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const truckController = new TruckController();
        sinon.stub(req,'get').withArgs('host').returns(false)
        sinon.stub(truckController, 'isAuthenticated').returns(true);
        sinon.stub(truckController, 'isAuthorized').returns(true);
    
        sinon.stub(truckController,'fetch').returns({
            status: 200,
            json: () => {
                return {
                    "message": "Truck updated"
                }
            }
        });
    
        //Act
        await truckController.editTruck(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 200);    
    });

    it('editTruck error returns JSON', async()=>{
        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {
            cookies:{
                jwt: 'token'
            },
            headers: {
                authorization: 'Bearer token',
                host: 'azure'                
            },
            get: function(header: string) {
                return this.headers[header];
            }
        };
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const truckController = new TruckController();
        sinon.stub(req,'get').withArgs('host').returns(false)
        sinon.stub(truckController, 'isAuthenticated').returns(true);
        sinon.stub(truckController, 'isAuthorized').returns(true);
    
        sinon.stub(truckController,'fetch').returns({
            status: 404,
            json: () => {
                return {
                    "message": "Error editing truck"
                }
            }
        });
    
        //Act
        await truckController.editTruck(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, sinon.match.has('message', 'Error editing truck'));    
    });

    it('editTruck error returns status', async()=>{
        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {
            cookies:{
                jwt: 'token'
            },
            headers: {
                authorization: 'Bearer token',
                host: 'azure'                
            },
            get: function(header: string) {
                return this.headers[header];
            }
        };
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const truckController = new TruckController();
        sinon.stub(req,'get').withArgs('host').returns(false)
        sinon.stub(truckController, 'isAuthenticated').returns(true);
        sinon.stub(truckController, 'isAuthorized').returns(true);
    
        sinon.stub(truckController,'fetch').returns({
            status: 404,
            json: () => {
                return {
                    "message": "Error editing truck"
                }
            }
        });
    
        //Act
        await truckController.editTruck(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 404);    
    });


    it('editTruckProlog returns JSON', async()=>{
        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {
            cookies:{
                jwt: 'token'
            },
            headers: {
                authorization: 'Bearer token',
                host: 'azure'                
            },
            get: function(header: string) {
                return this.headers[header];
            }
        };
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const truckController = new TruckController();
        sinon.stub(req,'get').withArgs('host').returns(false)
        sinon.stub(truckController, 'isAuthenticated').returns(true);
        sinon.stub(truckController, 'isAuthorized').returns(true);
    
        sinon.stub(truckController,'fetch').returns({
            status: 200,
            json: () => {
                return {
                    "message": "Truck updated"
                }
            }
        });
    
        //Act
        await truckController.editTruckProlog(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, sinon.match.has('message', 'Truck updated'));    
    });

    it('editTruckProlog returns status', async()=>{
        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {
            cookies:{
                jwt: 'token'
            },
            headers: {
                authorization: 'Bearer token',
                host: 'azure'                
            },
            get: function(header: string) {
                return this.headers[header];
            }
        };
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const truckController = new TruckController();
        sinon.stub(req,'get').withArgs('host').returns(false)
        sinon.stub(truckController, 'isAuthenticated').returns(true);
        sinon.stub(truckController, 'isAuthorized').returns(true);
    
        sinon.stub(truckController,'fetch').returns({
            status: 200,
            json: () => {
                return {
                    "message": "Truck updated"
                }
            }
        });
    
        //Act
        await truckController.editTruckProlog(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 200);    
    });

    it('editTruckProlog error returns JSON', async()=>{
        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {
            cookies:{
                jwt: 'token'
            },
            headers: {
                authorization: 'Bearer token',
                host: 'azure'                
            },
            get: function(header: string) {
                return this.headers[header];
            }
        };
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const truckController = new TruckController();
        sinon.stub(req,'get').withArgs('host').returns(false)
        sinon.stub(truckController, 'isAuthenticated').returns(true);
        sinon.stub(truckController, 'isAuthorized').returns(true);
    
        sinon.stub(truckController,'fetch').returns({
            status: 404,
            json: () => {
                return {
                    "message": "Error editing truck"
                }
            }
        });
    
        //Act
        await truckController.editTruckProlog(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, sinon.match.has('message', 'Error editing truck'));    
    });

    it('editTruckProlog error returns status', async()=>{
        //Arrange
        let body={
            
        };
    
        let req: Partial<Request> = {
            cookies:{
                jwt: 'token'
            },
            headers: {
                authorization: 'Bearer token',
                host: 'azure'                
            },
            get: function(header: string) {
                return this.headers[header];
            }
        };
        req.body = body;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const truckController = new TruckController();
        sinon.stub(req,'get').withArgs('host').returns(false)
        sinon.stub(truckController, 'isAuthenticated').returns(true);
        sinon.stub(truckController, 'isAuthorized').returns(true);
    
        sinon.stub(truckController,'fetch').returns({
            status: 404,
            json: () => {
                return {
                    "message": "Error editing truck"
                }
            }
        });
    
        //Act
        await truckController.editTruckProlog(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 404);    
    });


    it('softDeleteTruck returns JSON', async()=>{
        //Arrange
        let params={
            id: "1"
        };
    
        let req: Partial<Request> = {
            cookies:{
                jwt: 'token'
            },
            headers: {
                authorization: 'Bearer token',
                host: 'azure'                
            },
            get: function(header: string) {
                return this.headers[header];
            }
        };
        req.params = params;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const truckController = new TruckController();
        sinon.stub(req,'get').withArgs('host').returns(false)
        sinon.stub(truckController, 'isAuthenticated').returns(true);
        sinon.stub(truckController, 'isAuthorized').returns(true);
    
        sinon.stub(truckController,'fetch').returns({
            status: 200,
            json: () => {
                return {
                    "message": "Truck deleted"
                }
            }
        });
    
        //Act
        await truckController.softDeleteTruck(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, sinon.match.has('message', 'Truck deleted'));    
    });

    it('softDeleteTruck returns status', async()=>{
        //Arrange
        let params={
            id: "1"
        };
    
        let req: Partial<Request> = {
            cookies:{
                jwt: 'token'
            },
            headers: {
                authorization: 'Bearer token',
                host: 'azure'                
            },
            get: function(header: string) {
                return this.headers[header];
            }
        };
        req.params = params;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const truckController = new TruckController();
        sinon.stub(req,'get').withArgs('host').returns(false)
        sinon.stub(truckController, 'isAuthenticated').returns(true);
        sinon.stub(truckController, 'isAuthorized').returns(true);
    
        sinon.stub(truckController,'fetch').returns({
            status: 200,
            json: () => {
                return {
                    "message": "Truck deleted"
                }
            }
        });
    
        //Act
        await truckController.softDeleteTruck(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 200);    
    });

    it('softDeleteTruck error returns JSON', async()=>{
        //Arrange
        let params={
            id: "1"
        };
    
        let req: Partial<Request> = {
            cookies:{
                jwt: 'token'
            },
            headers: {
                authorization: 'Bearer token',
                host: 'azure'                
            },
            get: function(header: string) {
                return this.headers[header];
            }
        };
        req.params = params;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const truckController = new TruckController();
        sinon.stub(req,'get').withArgs('host').returns(false)
        sinon.stub(truckController, 'isAuthenticated').returns(true);
        sinon.stub(truckController, 'isAuthorized').returns(true);
    
        sinon.stub(truckController,'fetch').returns({
            status: 404,
            json: () => {
                return {
                    "message": "Error deleting truck"
                }
            }
        });
    
        //Act
        await truckController.softDeleteTruck(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, sinon.match.has('message', 'Error deleting truck'));    
    });

    it('softDeleteTruck error returns status', async()=>{
        //Arrange
        let params={
            id: "1"
        };
    
        let req: Partial<Request> = {
            cookies:{
                jwt: 'token'
            },
            headers: {
                authorization: 'Bearer token',
                host: 'azure'                
            },
            get: function(header: string) {
                return this.headers[header];
            }
        };
        req.params = params;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const truckController = new TruckController();
        sinon.stub(req,'get').withArgs('host').returns(false)
        sinon.stub(truckController, 'isAuthenticated').returns(true);
        sinon.stub(truckController, 'isAuthorized').returns(true);
    
        sinon.stub(truckController,'fetch').returns({
            status: 404,
            json: () => {
                return {
                    "message": "Error deleting truck"
                }
            }
        });
    
        //Act
        await truckController.softDeleteTruck(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 404);    
    });

    it('deleteTruckProlog returns JSON', async()=>{
        //Arrange
        let params={
            id: "1"
        };
    
        let req: Partial<Request> = {
            cookies:{
                jwt: 'token'
            },
            headers: {
                authorization: 'Bearer token',
                host: 'azure'                
            },
            get: function(header: string) {
                return this.headers[header];
            }
        };
        req.params = params;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const truckController = new TruckController();
        sinon.stub(req,'get').withArgs('host').returns(false)
        sinon.stub(truckController, 'isAuthenticated').returns(true);
        sinon.stub(truckController, 'isAuthorized').returns(true);
    
        sinon.stub(truckController,'fetch').returns({
            status: 200,
            json: () => {
                return {
                    "message": "Truck deleted"
                }
            }
        });
    
        //Act
        await truckController.deleteTruckProlog(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, sinon.match.has('message', 'Truck deleted'));    
    });

    it('deleteTruckProlog returns status', async()=>{
        //Arrange
        let params={
            id: "1"
        };
    
        let req: Partial<Request> = {
            cookies:{
                jwt: 'token'
            },
            headers: {
                authorization: 'Bearer token',
                host: 'azure'                
            },
            get: function(header: string) {
                return this.headers[header];
            }
        };
        req.params = params;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const truckController = new TruckController();
        sinon.stub(req,'get').withArgs('host').returns(false)
        sinon.stub(truckController, 'isAuthenticated').returns(true);
        sinon.stub(truckController, 'isAuthorized').returns(true);
    
        sinon.stub(truckController,'fetch').returns({
            status: 200,
            json: () => {
                return {
                    "message": "Truck deleted"
                }
            }
        });
    
        //Act
        await truckController.deleteTruckProlog(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 200);    
    });

    it('deleteTruckProlog error returns JSON', async()=>{
        //Arrange
        let params={
            id: "1"
        };
    
        let req: Partial<Request> = {
            cookies:{
                jwt: 'token'
            },
            headers: {
                authorization: 'Bearer token',
                host: 'azure'                
            },
            get: function(header: string) {
                return this.headers[header];
            }
        };
        req.params = params;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const truckController = new TruckController();
        sinon.stub(req,'get').withArgs('host').returns(false)
        sinon.stub(truckController, 'isAuthenticated').returns(true);
        sinon.stub(truckController, 'isAuthorized').returns(true);
    
        sinon.stub(truckController,'fetch').returns({
            status: 404,
            json: () => {
                return {
                    "message": "Error deleting truck"
                }
            }
        });
    
        //Act
        await truckController.deleteTruckProlog(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, sinon.match.has('message', 'Error deleting truck'));    
    });

    it('deleteTruckProlog error returns status', async()=>{
        //Arrange
        let params={
            id: "1"
        };
    
        let req: Partial<Request> = {
            cookies:{
                jwt: 'token'
            },
            headers: {
                authorization: 'Bearer token',
                host: 'azure'                
            },
            get: function(header: string) {
                return this.headers[header];
            }
        };
        req.params = params;
    
        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };
    
        let next: Partial<NextFunction>= () => {};

    
        const truckController = new TruckController();
        sinon.stub(req,'get').withArgs('host').returns(false)
        sinon.stub(truckController, 'isAuthenticated').returns(true);
        sinon.stub(truckController, 'isAuthorized').returns(true);
    
        sinon.stub(truckController,'fetch').returns({
            status: 404,
            json: () => {
                return {
                    "message": "Error deleting truck"
                }
            }
        });
    
        //Act
        await truckController.deleteTruckProlog(<Request>req,<Response>res,<NextFunction>next);
    
        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 404);    
    });



});





















