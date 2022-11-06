import "reflect-metadata";
import {Response, Request, NextFunction} from 'express';
import { Container } from 'typedi';
import { Result }  from '../../src/core/logic/Result';
import * as sinon from 'sinon';
import PathController from '../../src/controllers/PathController'
import IPathService from "../../src/services/IServices/IPathService";
import { IPathDTO } from "../../src/dto/IPathDTO";
import 'mocha'

describe('PathController Unit Tests', ()=>{
    const sandbox = sinon.createSandbox();
    beforeEach(()=>{
        Container.reset();

        let pathSchemaInstance = require('../../src/persistence/schemas/pathSchema').default;
        Container.set("pathSchema", pathSchemaInstance);

        let pathRepoClass = require('../../src/repos/pathRepo').default;
        let pathRepoInstance = Container.get(pathRepoClass);
        Container.set("PathRepo", pathRepoInstance);

        let pathServiceClass = require('../../src/services/pathService').default;
        let pathServiceInstance = Container.get(pathServiceClass);
        Container.set("PathService", pathServiceInstance);
    });

    afterEach(() => {
        sinon.restore();
        sandbox.restore();
    });

    it('createPath returns path JSON', async()=>{

        //Arrange
        let body={
            pathID : "path1",
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        };

        let req: Partial<Request> = {};
        req.body = body;

        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };

        let next: Partial<NextFunction>= () => {};

        let pathServiceInstance = Container.get("PathService");

        


        sinon.stub(pathServiceInstance,'createPath').returns(Promise.resolve(Result.ok<IPathDTO>(body as IPathDTO)));
        

        const pathController = new PathController(pathServiceInstance as IPathService);

        sinon.stub(pathController,'fetch').returns({
            status: 200
        });

        //Act
        await pathController.createPath(<Request>req,<Response>res,<NextFunction>next);

        //Assert
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json,sinon.match({
            pathID : "path1",
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        }));


    });

    it('createPath returns 409 when "Path Already Exists"', async() =>{
        
        //Arrange 
        let body={
            pathID : "path1",
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        };

        let req: Partial<Request> = {};
        req.body = body;

        let res: Partial<Response> = {
            status: sinon.spy(),
        };

        let next: Partial<NextFunction>= () => {};

        let pathServiceInstance = Container.get("PathService");

        sinon.stub(pathServiceInstance,'createPath').returns(Promise.resolve(Result.fail<IPathDTO>("Path already exists")));

        const pathController = new PathController(pathServiceInstance as IPathService);

        
        sinon.stub(pathController,'fetch').returns({
            status: 200
        });


        //act
        await pathController.createPath(<Request>req, <Response>res, <NextFunction>next)

        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status,409);
    });

    it('getPath returns path JSON', async()=>{

        //Arrange
        let body={
            pathID : "path1",
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        };

        let req: Partial<Request> = {};
        req.body = {
            pathID:"pathID"
        };

        let res: Partial<Response> = {
            json:sinon.spy(),
            status: sinon.spy(),
        };

        let next: Partial<NextFunction>= () => {};

        let pathServiceInstance = Container.get("PathService");

        sinon.stub(pathServiceInstance,'getPath').returns(Promise.resolve(Result.ok<IPathDTO>(body as IPathDTO)));

        const pathController = new PathController(pathServiceInstance as IPathService)

        //Act 
        await pathController.getPath(<Request>req, <Response>res, <NextFunction>next)

        //Assert
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, sinon.match(body));
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 200);

    });

    it('getPath returns path JSON', async()=>{

        //Arrange
        let body={
            pathID : "path1",
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        };

        let req: Partial<Request> = {};
        req.body = body;

        let res: Partial<Response> = {
            status: sinon.spy(),
        };

        let next: Partial<NextFunction>= () => {};

        let pathServiceInstance = Container.get("PathService");

        sinon.stub(pathServiceInstance,'getPath').returns(Promise.resolve(Result.ok<IPathDTO>(body as IPathDTO)));

        const pathController = new PathController(pathServiceInstance as IPathService);

        //Act 
        await pathController.getPath(<Request>req, <Response>res, <NextFunction>next);

         //Assert
         sinon.assert.calledOnce(res.status);
         sinon.assert.calledWith(res.status, 200);

    });

    it('getPath returns "Path not found"', async() => {
        
        //Arrange 
        let req: Partial<Request>= {};
        req.body={
            pathID: "pathID"
        };

        let res: Partial<Response> ={
            status : sinon.spy(),
            send: sinon.spy()
        };

        let next : Partial<NextFunction> = () => {};

        let pathServiceInstance = Container.get("PathService");

        sinon.stub(pathServiceInstance,'getPath').returns(Promise.resolve(Result.fail<IPathDTO>("Path not found")));

        const pathController = new PathController(pathServiceInstance as IPathService);

        //Act
        await pathController.getPath(<Request>req,<Response>res,<NextFunction>next);

        //Assert
        sinon.assert.calledOnce(res.send);
        sinon.assert.calledWith(res.send,sinon.match("Path not found"));
    })

    it('getPath returns 404', async() => {
        
        //Arrange 
        let req: Partial<Request>= {};
        req.body={
            pathID: "pathID"
        };

        let res: Partial<Response> ={
            status : sinon.spy(),
        };

        let next : Partial<NextFunction> = () => {};

        let pathServiceInstance = Container.get("PathService");

        sinon.stub(pathServiceInstance,'getPath').returns(Promise.resolve(Result.fail<IPathDTO>("Path not found")));

        const pathController = new PathController(pathServiceInstance as IPathService);

        //Act
        await pathController.getPath(<Request>req,<Response>res,<NextFunction>next);

        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status,404);
    });

    it('getAllPaths returns Paths', async()=>{
        //Arrange
        let body=[{
            pathID : "path1",
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        },{
            pathID : "path2",
            startWHId : 'WH6',
            destinationWHId: 'WH5',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        }];

        let req: Partial<Request> = {};
        req.body={

        }

        let res: Partial<Response> ={
            status : sinon.spy(),
            json: sinon.spy()
        };

        let next: Partial<NextFunction> = () => {};

        let pathServiceInstance = Container.get("PathService");

        sinon.stub(pathServiceInstance,'getAllPath').returns(Promise.resolve(Result.ok<IPathDTO[]>(body as IPathDTO[])));

        const pathController = new PathController(pathServiceInstance as IPathService);

        //Act
        await pathController.getAllPaths(<Request>req, <Response>res, <NextFunction>next);

        //Assert
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, body);
    });

    
    it('getAllPaths START WH returns Paths', async()=>{
        //Arrange
        let body=[{
            pathID : "path1",
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        },{
            pathID : "path2",
            startWHId : 'WH6',
            destinationWHId: 'WH5',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        }];

        let req: Partial<Request> = {};
        req.body={
            startWhId: 'WH6'
        }

        let res: Partial<Response> ={
            status : sinon.spy(),
            json: sinon.spy()
        };

        let next: Partial<NextFunction> = () => {};

        let pathServiceInstance = Container.get("PathService");

        sinon.stub(pathServiceInstance,'getAllPath').returns(Promise.resolve(Result.ok<IPathDTO[]>(body as IPathDTO[])));

        const pathController = new PathController(pathServiceInstance as IPathService);

        //Act
        await pathController.getAllPaths(<Request>req, <Response>res, <NextFunction>next);

        //Assert
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, body);
    });

    it('getAllPaths Dest WH returns Paths', async()=>{
        //Arrange
        let body=[{
            pathID : "path1",
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        },{
            pathID : "path2",
            startWHId : 'WH6',
            destinationWHId: 'WH5',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        }];

        let req: Partial<Request> = {};
        req.body={
            destinationWHId: 'WH6'
        }

        let res: Partial<Response> ={
            status : sinon.spy(),
            json: sinon.spy()
        };

        let next: Partial<NextFunction> = () => {};

        let pathServiceInstance = Container.get("PathService");

        sinon.stub(pathServiceInstance,'getAllPath').returns(Promise.resolve(Result.ok<IPathDTO[]>(body as IPathDTO[])));

        const pathController = new PathController(pathServiceInstance as IPathService);

        //Act
        await pathController.getAllPaths(<Request>req, <Response>res, <NextFunction>next);

        //Assert
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, body);
    });

    it('getAllPaths START WH AND DEST WH returns Paths', async()=>{
        //Arrange
        let body=[{
            pathID : "path1",
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        },{
            pathID : "path2",
            startWHId : 'WH6',
            destinationWHId: 'WH5',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        }];

        let req: Partial<Request> = {};
        req.body={
            startWhId: 'WH6',
            destinationWHId: 'WH5'
        }

        let res: Partial<Response> ={
            status : sinon.spy(),
            json: sinon.spy()
        };

        let next: Partial<NextFunction> = () => {};

        let pathServiceInstance = Container.get("PathService");

        sinon.stub(pathServiceInstance,'getAllPath').returns(Promise.resolve(Result.ok<IPathDTO[]>(body as IPathDTO[])));

        const pathController = new PathController(pathServiceInstance as IPathService);

        //Act
        await pathController.getAllPaths(<Request>req, <Response>res, <NextFunction>next);

        //Assert
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, body);
    });

    
    it('getAllPaths returns 200', async()=>{
        //Arrange
        let body=[{
            pathID : "path1",
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        },{
            pathID : "path2",
            startWHId : 'WH6',
            destinationWHId: 'WH5',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        }];

        let req: Partial<Request> = {};

        let res: Partial<Response> ={
            status : sinon.spy(),
            json: sinon.spy()
        };

        let next: Partial<NextFunction> = () => {};

        let pathServiceInstance = Container.get("PathService");

        sinon.stub(pathServiceInstance,'getAllPath').returns(Promise.resolve(Result.ok<IPathDTO[]>(body as IPathDTO[])));

        const pathController = new PathController(pathServiceInstance as IPathService);

        //Act
        await pathController.getAllPaths(<Request>req, <Response>res, <NextFunction>next);

        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 200);
    });

    it('deletePath returns path', async() => {

        //Arrange 
        let body={
            pathID : "path1",
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        };

        let req: Partial<Request> = {};
        req.body = {
            pathID: "pathID"
        };

        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };

        let next: Partial<NextFunction>= () => {};

        let pathServiceInstance = Container.get("PathService");

        sinon.stub(pathServiceInstance,'deletePath').returns(Promise.resolve(Result.ok<IPathDTO>(body as IPathDTO)));

        const pathController = new PathController(pathServiceInstance as IPathService);

        //Act
        await pathController.deletePath(<Request>req, <Response>res, <NextFunction>next);

        //Assert
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, body);

    });

    it('deletePath returns 200', async() => {

        //Arrange 
        let body={
            pathID : "path1",
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        };

        let req: Partial<Request> = {};
        req.body = {
            pathID: "pathID"
        };

        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };

        let next: Partial<NextFunction>= () => {};

        let pathServiceInstance = Container.get("PathService");

        sinon.stub(pathServiceInstance,'deletePath').returns(Promise.resolve(Result.ok<IPathDTO>(body as IPathDTO)));

        const pathController = new PathController(pathServiceInstance as IPathService);

        //Act
        await pathController.deletePath(<Request>req, <Response>res, <NextFunction>next);

        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 200);

    });

    it('deletePath returns "Path not found"', async() => {

        //Arrange 
        let body={
            pathID : "path1",
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        };

        let req: Partial<Request> = {};
        req.body = {
            pathID: "pathID"
        };

        let res: Partial<Response> = {
            status: sinon.spy(),
            send: sinon.spy()
        };

        let next: Partial<NextFunction>= () => {};

        let pathServiceInstance = Container.get("PathService");

        sinon.stub(pathServiceInstance,'deletePath').returns(Promise.resolve(Result.fail<IPathDTO>("Path not found")));

        const pathController = new PathController(pathServiceInstance as IPathService);

        //Act
        await pathController.deletePath(<Request>req, <Response>res, <NextFunction>next);

        //Assert
        sinon.assert.calledOnce(res.send);
        sinon.assert.calledWith(res.send, "Path not found");

    });

    it('deletePath returns 404', async() => {

        //Arrange 
    
        let req: Partial<Request> = {};
        req.body = {
            pathID: "pathID"
        };

        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };

        let next: Partial<NextFunction>= () => {};

        let pathServiceInstance = Container.get("PathService");

        sinon.stub(pathServiceInstance,'deletePath').returns(Promise.resolve(Result.fail<IPathDTO>("Path not found")));

        const pathController = new PathController(pathServiceInstance as IPathService);

        //Act
        await pathController.deletePath(<Request>req, <Response>res, <NextFunction>next);

        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 404);

    });

    it('update return path', async()=>{

        //Arrange 
        let body={
            pathID : "path1",
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        };

        let req: Partial<Request> = {};
        req.body = body;

        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };

        let next: Partial<NextFunction>= () => {};

        let pathServiceInstance = Container.get("PathService");

        sinon.stub(pathServiceInstance,'updatePath').returns(Promise.resolve(Result.ok<IPathDTO>(body as IPathDTO)));
        
        const pathController = new PathController(pathServiceInstance as IPathService);

        //Act
        await pathController.updatePath(<Request>req, <Response>res, <NextFunction>next);

        //Assert
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, body);
    });

    it('update return 200', async()=>{

        //Arrange 
        let body={
            pathID : "path1",
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        };

        let req: Partial<Request> = {};
        req.body = body;

        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };

        let next: Partial<NextFunction>= () => {};

        let pathServiceInstance = Container.get("PathService");

        sinon.stub(pathServiceInstance,'updatePath').returns(Promise.resolve(Result.ok<IPathDTO>(body as IPathDTO)));
        
        const pathController = new PathController(pathServiceInstance as IPathService);

        //Act
        await pathController.updatePath(<Request>req, <Response>res, <NextFunction>next);

        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 200);
    });

    it('update return "Path not found"', async()=>{

        //Arrange 
        let body={
            pathID : "path1",
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        };

        let req: Partial<Request> = {};
        req.body = body;

        let res: Partial<Response> = {
            status: sinon.spy(),
            send: sinon.spy()
        };

        let next: Partial<NextFunction>= () => {};

        let pathServiceInstance = Container.get("PathService");

        sinon.stub(pathServiceInstance,'updatePath').returns(Promise.resolve(Result.fail<IPathDTO>("Path not found")));
        
        const pathController = new PathController(pathServiceInstance as IPathService);

        //Act
        await pathController.updatePath(<Request>req, <Response>res, <NextFunction>next);

        //Assert
        sinon.assert.calledOnce(res.send);
        sinon.assert.calledWith(res.send, "Path not found");
    });

    it('update return 404', async()=>{

        //Arrange 
        let body={
            pathID : "path1",
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        };

        let req: Partial<Request> = {};
        req.body = body;

        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };

        let next: Partial<NextFunction>= () => {};

        let pathServiceInstance = Container.get("PathService");

        sinon.stub(pathServiceInstance,'updatePath').returns(Promise.resolve(Result.fail<IPathDTO>("Path not found")));
        
        const pathController = new PathController(pathServiceInstance as IPathService);

        //Act
        await pathController.updatePath(<Request>req, <Response>res, <NextFunction>next);

        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 404);
    });









});