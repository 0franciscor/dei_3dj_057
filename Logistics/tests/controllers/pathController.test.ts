import "reflect-metadata";
import {Response, Request, NextFunction} from 'express';
import { Container } from 'typedi';
import { Result }  from '../../src/core/logic/Result';
import * as sinon from 'sinon';
import PathController from '../../src/controllers/pathController'
import IPathService from "../../src/services/IServices/IPathService";
import { IPathDTO } from "../../src/dto/IPathDTO";
import 'mocha'
import {PathMap} from "../../src/mappers/PathMap"
import path from "path";
import {Path} from '../../src/domain/path/Path'
import { IPathPersistance } from "../../src/dataschema/IPathPersistance";
import { Document, FilterQuery, Model } from 'mongoose';

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

        let req: Partial<Request> = {
            headers: {
                cookie: "cookie"
            }
        };
        
        req.body = body;

        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };

        let next: Partial<NextFunction>= () => {};

        let pathServiceInstance = Container.get("PathService");

        


        sinon.stub(pathServiceInstance,'createPath').returns(Promise.resolve(Result.ok<IPathDTO>(body as IPathDTO)));
        

        const pathController = new PathController(pathServiceInstance as IPathService)
        sinon.stub(pathController, 'isAuthenticated').returns(true);
        sinon.stub(pathController, 'isAuthorized').returns(true);

        sinon.stub(pathController,'fetch').returns(Promise.resolve({
            status: 200
        }));

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

        let req: Partial<Request> = {
            headers: {
                cookie: "cookie"
            }
        };
        req.body = body;

        let res: Partial<Response> = {
            status: sinon.spy(),
        };

        let next: Partial<NextFunction>= () => {};

        let pathServiceInstance = Container.get("PathService");

        sinon.stub(pathServiceInstance,'createPath').returns(Promise.resolve(Result.fail<IPathDTO>("Path already exists")));

        const pathController = new PathController(pathServiceInstance as IPathService)
        sinon.stub(pathController, 'isAuthenticated').returns(true);
        sinon.stub(pathController, 'isAuthorized').returns(true);

        
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
        sinon.stub(pathController, 'isAuthenticated').returns(true);
        sinon.stub(pathController, 'isAuthorized').returns(true);

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

        const pathController = new PathController(pathServiceInstance as IPathService)
        sinon.stub(pathController, 'isAuthenticated').returns(true);
        sinon.stub(pathController, 'isAuthorized').returns(true);

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

        const pathController = new PathController(pathServiceInstance as IPathService)
        sinon.stub(pathController, 'isAuthenticated').returns(true);
        sinon.stub(pathController, 'isAuthorized').returns(true);

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

        const pathController = new PathController(pathServiceInstance as IPathService)
        sinon.stub(pathController, 'isAuthenticated').returns(true);
        sinon.stub(pathController, 'isAuthorized').returns(true);

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
        req.params={

        }

        let res: Partial<Response> ={
            status : sinon.spy(),
            json: sinon.spy()
        };

        let next: Partial<NextFunction> = () => {};

        let pathServiceInstance = Container.get("PathService");

        sinon.stub(pathServiceInstance,'getAllPath').returns(Promise.resolve(Result.ok<IPathDTO[]>(body as IPathDTO[])));

        const pathController = new PathController(pathServiceInstance as IPathService)
        sinon.stub(pathController, 'isAuthenticated').returns(true);
        sinon.stub(pathController, 'isAuthorized').returns(true);

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
        req.params={
            startWHId : 'WH5',
        }

        let res: Partial<Response> ={
            status : sinon.spy(),
            json: sinon.spy()
        };

        let next: Partial<NextFunction> = () => {};

        let pathServiceInstance = Container.get("PathService");

        sinon.stub(pathServiceInstance,'getAllPath').returns(Promise.resolve(Result.ok<IPathDTO[]>(body as IPathDTO[])));

        const pathController = new PathController(pathServiceInstance as IPathService)
        sinon.stub(pathController, 'isAuthenticated').returns(true);
        sinon.stub(pathController, 'isAuthorized').returns(true);

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
        req.params={
            destinationWHId: 'WH6'
        }

        let res: Partial<Response> ={
            status : sinon.spy(),
            json: sinon.spy()
        };

        let next: Partial<NextFunction> = () => {};

        let pathServiceInstance = Container.get("PathService");

        sinon.stub(pathServiceInstance,'getAllPath').returns(Promise.resolve(Result.ok<IPathDTO[]>(body as IPathDTO[])));

        const pathController = new PathController(pathServiceInstance as IPathService)
        sinon.stub(pathController, 'isAuthenticated').returns(true);
        sinon.stub(pathController, 'isAuthorized').returns(true);

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
        req.params={
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

        const pathController = new PathController(pathServiceInstance as IPathService)
        sinon.stub(pathController, 'isAuthenticated').returns(true);
        sinon.stub(pathController, 'isAuthorized').returns(true);

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
        req.params={

        }

        let res: Partial<Response> ={
            status : sinon.spy(),
            json: sinon.spy()
        };

        let next: Partial<NextFunction> = () => {};

        let pathServiceInstance = Container.get("PathService");

        sinon.stub(pathServiceInstance,'getAllPath').returns(Promise.resolve(Result.ok<IPathDTO[]>(body as IPathDTO[])));

        const pathController = new PathController(pathServiceInstance as IPathService)
        sinon.stub(pathController, 'isAuthenticated').returns(true);
        sinon.stub(pathController, 'isAuthorized').returns(true);

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

        const pathController = new PathController(pathServiceInstance as IPathService)
        sinon.stub(pathController, 'isAuthenticated').returns(true);
        sinon.stub(pathController, 'isAuthorized').returns(true);

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

        const pathController = new PathController(pathServiceInstance as IPathService)
        sinon.stub(pathController, 'isAuthenticated').returns(true);
        sinon.stub(pathController, 'isAuthorized').returns(true);

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

        const pathController = new PathController(pathServiceInstance as IPathService)
        sinon.stub(pathController, 'isAuthenticated').returns(true);
        sinon.stub(pathController, 'isAuthorized').returns(true);

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

        const pathController = new PathController(pathServiceInstance as IPathService)
        sinon.stub(pathController, 'isAuthenticated').returns(true);
        sinon.stub(pathController, 'isAuthorized').returns(true);

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
        
        const pathController = new PathController(pathServiceInstance as IPathService)
        sinon.stub(pathController, 'isAuthenticated').returns(true);
        sinon.stub(pathController, 'isAuthorized').returns(true);

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
        
        const pathController = new PathController(pathServiceInstance as IPathService)
        sinon.stub(pathController, 'isAuthenticated').returns(true);
        sinon.stub(pathController, 'isAuthorized').returns(true);

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
        
        const pathController = new PathController(pathServiceInstance as IPathService)
        sinon.stub(pathController, 'isAuthenticated').returns(true);
        sinon.stub(pathController, 'isAuthorized').returns(true);

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
        
        const pathController = new PathController(pathServiceInstance as IPathService)
        sinon.stub(pathController, 'isAuthenticated').returns(true);
        sinon.stub(pathController, 'isAuthorized').returns(true);

        //Act
        await pathController.updatePath(<Request>req, <Response>res, <NextFunction>next);

        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 404);
    });









});

describe('PathController + PathService Integration tests ', () =>{
    const sandbox = sinon.createSandbox();
    beforeEach(() => {
        Container.reset();

        let pathSchemaInstance = require('../../src/persistence/schemas/pathSchema').default;
        Container.set("pathSchema",pathSchemaInstance);

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

    it('createPath returns path', async()=>{
        //Arrange
        let body={
            id:"id",
            pathID : "path1",
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        };

        let req: Partial<Request> = {
            headers: {
                cookie: "cookie"
            }
        };
            req.body = body;

        let res: Partial<Response> = {
            status: sinon.spy(),
        };

        let next: Partial<NextFunction>= () => {};

        let pathRepoInstance = Container.get("PathRepo");
        sinon.stub(pathRepoInstance,'getAllPaths').returns(Promise.resolve([]));
        sinon.stub(pathRepoInstance, 'getPathById').returns(null);
        sinon.stub(pathRepoInstance,'save').returns(Promise.resolve(Result.ok<IPathDTO>(body as IPathDTO)));

        let pathServiceInstance = Container.get("PathService");
        const pathServiceSpy = sinon.spy(pathServiceInstance,'createPath');

        const pathController = new PathController(pathServiceInstance as IPathService)
        sinon.stub(pathController, 'isAuthenticated').returns(true);
        sinon.stub(pathController, 'isAuthorized').returns(true);

        sinon.stub(pathController,'fetch').returns({
            status: 200
        });

        //Act
        await pathController.createPath(<Request>req, <Response>res, <NextFunction>next);

        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 201);
        sinon.assert.calledOnce(pathServiceSpy);
        sinon.assert.calledWith(pathServiceSpy,body);
    });

    it('updatePath returns Path', async() => {
        //Arrange 
        let body={
            id:"id",
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

        let pathRepoInstance = Container.get("PathRepo");
        sinon.stub(pathRepoInstance, 'getPathById').returns(Promise.resolve(PathMap.toDomain(body as IPathDTO)));
        sinon.stub(pathRepoInstance,'save').returns(Promise.resolve(Result.ok<IPathDTO>(body as IPathDTO)));

        let pathServiceInstance = Container.get("PathService");
        const pathServiceSpy = sinon.spy(pathServiceInstance,'updatePath');

        const pathController = new PathController(pathServiceInstance as IPathService)
        sinon.stub(pathController, 'isAuthenticated').returns(true);
        sinon.stub(pathController, 'isAuthorized').returns(true);
        
        //Act
        await pathController.updatePath(<Request>req, <Response>res, <NextFunction>next);

        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status,200);
        sinon.assert.calledOnce(pathServiceSpy);
        sinon.assert.calledWith(pathServiceSpy,body);
    });

    it('deletePath returns path', async()=>{
        //Arrange 
        let body={
            id:"id",
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

        let pathRepoInstance = Container.get("PathRepo");
        sinon.stub(pathRepoInstance, 'getPathById').returns(Promise.resolve(PathMap.toDomain(body as IPathDTO)));
        sinon.stub(pathRepoInstance,'delete').returns(Promise.resolve(PathMap.toDomain(body as IPathDTO)));

        let pathServiceInstance = Container.get("PathService");
        const pathServiceSpy = sinon.spy(pathServiceInstance,'deletePath');

        const pathController = new PathController(pathServiceInstance as IPathService)
        sinon.stub(pathController, 'isAuthenticated').returns(true);
        sinon.stub(pathController, 'isAuthorized').returns(true);

        //Act
        await pathController.deletePath(<Request>req, <Response>res, <NextFunction>next);

        //Assert 
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 200);
        sinon.assert.calledOnce(pathServiceSpy);
        sinon.assert.calledWith(pathServiceSpy,body.id);
    });


    it('getPath returns Path', async()=>{
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

        let pathRepoInstance = Container.get("PathRepo");
        sinon.stub(pathRepoInstance, 'getPathById').returns(Promise.resolve(PathMap.toDomain(body as IPathDTO)));

        let pathServiceInstance = Container.get("PathService");
        const pathServiceSpy = sinon.spy(pathServiceInstance,'getPath');

        const pathController = new PathController(pathServiceInstance as IPathService)
        sinon.stub(pathController, 'isAuthenticated').returns(true);
        sinon.stub(pathController, 'isAuthorized').returns(true);

        //Act
        await pathController.getPath(<Request>req, <Response>res, <NextFunction>next);

        //Assert 
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 200);
        sinon.assert.calledOnce(pathServiceSpy);
        sinon.assert.calledWith(pathServiceSpy,"pathID");


    })

    it('getAllPaths returns List', async()=>{
        //Arrange 
        let body=[{
            id: "id1",
            pathID : "path1",
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        },{
            id:"id2",
            pathID : "path2",
            startWHId : 'WH6',
            destinationWHId: 'WH5',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        }];

        let req: Partial<Request> = {};
            req.params = {
               
            };

        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy()
        };

        let next: Partial<NextFunction>= () => {};

        let pathRepoInstance = Container.get("PathRepo");
        let paths: Path[]=[];
        body.forEach(path=>{
            paths.push(PathMap.toDomain(path));
        });
        sinon.stub(pathRepoInstance,"getAllPaths").returns(Promise.resolve(body));

        let pathServiceInstance = Container.get("PathService");
        const pathServiceSpy = sinon.spy(pathServiceInstance,'getAllPath');

        const pathController = new PathController(pathServiceInstance as IPathService)
        sinon.stub(pathController, 'isAuthenticated').returns(true);
        sinon.stub(pathController, 'isAuthorized').returns(true);

        //Act
        await pathController.getAllPaths(<Request>req, <Response>res, <NextFunction>next);

        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 200);
        sinon.assert.calledOnce(pathServiceSpy);
        sinon.assert.calledWith(pathServiceSpy)

    });


})

describe("PathController + PathService + PathRepo Integration tests", ()=>{

    const sandbox = sinon.createSandbox();
    beforeEach(() => {
        Container.reset();

        let pathSchemaInstance = require('../../src/persistence/schemas/pathSchema').default;
        Container.set("pathSchema",pathSchemaInstance);

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

    it('createPath returns Path', async() =>{

        //Arrange

        let body={
            id: "id1",
            pathID : "path1",
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        };

        let body2={
            domainId:"id2",
            pathID : "path2",
            startWHId : 'WH6',
            destinationWHId: 'WH5',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        } as IPathPersistance;

        let req: Partial<Request> = {
            headers: {
                cookie: "cookie"
            }
        };
            req.body=body;

        let res: Partial<Response> = {
            status: sinon.spy(),
            json: sinon.spy(),
            send: sinon.spy()
        };

        let next: Partial<NextFunction> = () => {};

        let pathSchemaInstance= Container.get("pathSchema");
        sinon.stub(pathSchemaInstance,'find').returns(Promise.resolve([]));
        sinon.stub(pathSchemaInstance,'findOne').returns(Promise.resolve(null));
        sinon.stub(pathSchemaInstance,'create').returns(Promise.resolve(body2 as IPathPersistance));

        let pathRepoInstance = Container.get("PathRepo");
        const pathRepoSpy = sinon.spy(pathRepoInstance,'save');

        let pathServiceInstance = Container.get("PathService");
        const pathServiceSpy = sinon.spy(pathServiceInstance,'createPath');

        const pathController = new PathController(pathServiceInstance as IPathService)
        sinon.stub(pathController, 'isAuthenticated').returns(true);
        sinon.stub(pathController, 'isAuthorized').returns(true);

        sinon.stub(pathController,'fetch').returns({
            status: 200
        });

        //Act
        await pathController.createPath(<Request>req, <Response>res, <NextFunction>next);

        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 201);
        sinon.assert.calledOnce(pathRepoSpy);
        sinon.assert.calledWith(pathServiceSpy);
    });

    it('updatePath returns Path', async() => {
        let body={
            id: "id1",
            pathID : "path1",
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        };

        const body2={
            id: "id1",
            pathID : "path1",
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30,
            save(){return this;}
        } as IPathPersistance& Document<any, any, any>;

        let req: Partial<Request> = {};
        req.body=body;

        let res: Partial<Response> = {
            status: sinon.spy()
        };

        let next: Partial<NextFunction> = () => {};

        let pathSchemaInstance= Container.get("pathSchema");
        sinon.stub(pathSchemaInstance,'findOne').returns(Promise.resolve(body2 as IPathPersistance & Document<any,any,any>));

        let pathRepoInstance = Container.get("PathRepo");
        const pathRepoSpy = sinon.spy(pathRepoInstance,'save')

        let pathServiceInstance = Container.get("PathService");
        const pathServiceSpy = sinon.spy(pathServiceInstance,'updatePath');

        const pathController = new PathController(pathServiceInstance as IPathService)
        sinon.stub(pathController, 'isAuthenticated').returns(true);
        sinon.stub(pathController, 'isAuthorized').returns(true);

        //Act
        await pathController.updatePath(<Request>req, <Response>res, <NextFunction>next);

        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 200);
        sinon.assert.calledOnce(pathRepoSpy);
        sinon.assert.calledWith(pathServiceSpy);

    });

    it('deletePath returns Path', async()=>{
        let body={
            id: "id1",
            pathID : "path1",
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        };

        let body2={
            domainId:"id2",
            pathID : "path2",
            startWHId : 'WH6',
            destinationWHId: 'WH5',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        } as IPathPersistance;

        let req: Partial<Request> = {};
        req.body=body;

        let res: Partial<Response> = {
            status: sinon.spy()
        };

        let next: Partial<NextFunction> = () => {};

        let pathSchemaInstance= Container.get("pathSchema");
        sinon.stub(pathSchemaInstance,'findOne').returns(Promise.resolve(body2 as IPathPersistance & Document<any,any,any>));
        sinon.stub(pathSchemaInstance,'deleteOne').returns(Promise.resolve(body2 as IPathPersistance & Document<any,any,any> ));

        let pathRepoInstance = Container.get("PathRepo");
        const pathRepoSpy = sinon.spy(pathRepoInstance,'delete')

        let pathServiceInstance = Container.get("PathService");
        const pathServiceSpy = sinon.spy(pathServiceInstance,'deletePath');

        const pathController = new PathController(pathServiceInstance as IPathService)
        sinon.stub(pathController, 'isAuthenticated').returns(true);
        sinon.stub(pathController, 'isAuthorized').returns(true);

        //Act
        await pathController.deletePath(<Request>req, <Response>res, <NextFunction>next);

        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 200);
        sinon.assert.calledOnce(pathRepoSpy);
        sinon.assert.calledWith(pathServiceSpy);
    });

    it('getPath returns path', async() =>{
        let body={
            id: "id1",
            pathID : "path1",
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        };

        let body2={
            domainId:"id2",
            pathID : "path2",
            startWHId : 'WH6',
            destinationWHId: 'WH5',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        } as IPathPersistance;

        let req: Partial<Request> = {};
        req.body=body;

        let res: Partial<Response> = {
            status: sinon.spy()
        };

        let next: Partial<NextFunction> = () => {};

        let pathSchemaInstance= Container.get("pathSchema");
        sinon.stub(pathSchemaInstance,'findOne').returns(Promise.resolve(body2 as IPathPersistance & Document<any,any,any>));

        let pathRepoInstance = Container.get("PathRepo");
        const pathRepoSpy = sinon.spy(pathRepoInstance,'getPathById')

        let pathServiceInstance = Container.get("PathService");
        const pathServiceSpy = sinon.spy(pathServiceInstance,'getPath');

        const pathController = new PathController(pathServiceInstance as IPathService)
        sinon.stub(pathController, 'isAuthenticated').returns(true);
        sinon.stub(pathController, 'isAuthorized').returns(true);

        //Act
        await pathController.getPath(<Request>req, <Response>res, <NextFunction>next);

        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 200);
        sinon.assert.calledOnce(pathRepoSpy);
        sinon.assert.calledWith(pathServiceSpy);

    
    });

    it('getAllPaths returns list', async()=> {
        let body=[{
            id: "id1",
            pathID : "path1",
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        },{
            id:"id2",
            pathID : "path2",
            startWHId : 'WH6',
            destinationWHId: 'WH5',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        } ];

        let paths: Path [] = [];
        body.forEach(path => {
            paths.push(PathMap.toDomain(path));
        });

        let body2={
            domainId:"id",
            pathID : "pathID",
            startWHId : 'WH7',
            destinationWHId: 'WH5',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        } as IPathPersistance;

        let req: Partial<Request> = {};
        req.body=body; 
        req.params={
            
        }

        let res: Partial<Response> = {
            status: sinon.spy()
        };

        let next: Partial<NextFunction> = () => {};

        let pathSchemaInstance= Container.get("pathSchema");
        sinon.stub(pathSchemaInstance,'find').returns(Promise.resolve(body));

        let pathRepoInstance = Container.get("PathRepo");
        const pathRepoSpy = sinon.spy(pathRepoInstance,'getAllPaths')

        let pathServiceInstance = Container.get("PathService");
        const pathServiceSpy = sinon.spy(pathServiceInstance,'getAllPath');

        const pathController = new PathController(pathServiceInstance as IPathService)
        sinon.stub(pathController, 'isAuthenticated').returns(true);
        sinon.stub(pathController, 'isAuthorized').returns(true);

        //Act
        await pathController.getAllPaths(<Request>req, <Response>res, <NextFunction>next);

        //Assert
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 200);
        sinon.assert.calledOnce(pathRepoSpy);
        sinon.assert.calledWith(pathServiceSpy);

    })



});