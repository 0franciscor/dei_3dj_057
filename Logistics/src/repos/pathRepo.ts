import { Document,FilterQuery,Model } from "mongoose";
import { Inject, Service } from "typedi";
import IPathRepo from './IRepos/IPathRepo'
import { IPathPersistance } from "../dataschema/IPathPersistance";
import { Path } from "../domain/path/Path";
import { PathID } from '../domain/path/PathID';
import { PathMap } from "../mappers/PathMap";
import { start } from "repl";
import { DestinationWHId } from "../domain/path/DestinationWHId";
import { StartWHId } from "../domain/path/StartWHId";
import { Result } from "../core/logic/Result";
import path from "path";


@Service()
export default class PathRepo implements IPathRepo{
    constructor(
        @Inject('pathSchema') private pathSchema : Model<IPathPersistance & Document>,
    ) {}
      
    public async exists(path: Path): Promise<boolean> {
        const idX= path.id instanceof PathID ? (<PathID>path.id):path.id;
        
        const query = {domainId: idX};
        const PathDocument = await this.pathSchema.findById(query as
        FilterQuery<IPathPersistance & Document>);

        return !!PathDocument === true;
    }

    public async save(path:Path): Promise<Path>{
        const query= {pathID: path.pathID.id};
        const PathDocument = await this.pathSchema.findOne(query as FilterQuery<IPathPersistance & Document>);
        try{
            if (PathDocument === null){
                const rawPath: any = PathMap.toPersistance(path);
                const pathCreated = await this.pathSchema.create(rawPath);
                return PathMap.toDomain(pathCreated);
            }
            else{
                PathDocument.pathID= path.pathID.id;
                PathDocument.startWHId= path.startWHId.startWHId;
                PathDocument.destinationWHId= path.destinationWHId.destinationWHId;
                PathDocument.pathTravelTime= path.pathTravelTime.pathTravelTime;
                PathDocument.wastedEnergy= path.wastedEnergy.wastedEnergy;
                PathDocument.extraTravelTime= path.extraTravelTime.extraTravelTime;
                await PathDocument.save();
                return path;
            }
        }catch(error){
            throw error;
        }
    }
    public async delete (pathID: PathID): Promise<Path>{
        const query = {pathId:pathID.id};
        console.log(query);
        const pathDocument = await this.pathSchema.findOne(query as 
        FilterQuery<IPathPersistance & Document>); 
        try {
            if(pathDocument === null){
                return null;
            }
            else{
                await this.pathSchema.deleteOne(query as 
                FilterQuery<IPathPersistance & Document>);
                return null;
            }
        }catch(error){
            throw error;
        }
    }
        
        public async getPathById(pathID: string): Promise<Path> {
            const query ={pathID: pathID}
            const pathDocument = await this.pathSchema.findOne(query as 
            FilterQuery<IPathPersistance & Document>);
            console.log(pathDocument);

            if(pathDocument === null){
                return null;
            }
            else{
                return PathMap.toDomain(pathDocument);
            }
        }
        
        public async getAllPaths(startWH:string ,destinationWH:string): Promise<Path[]> {
            let query;
            let pathDocument;
           
            if(startWH==undefined){
                 query ={destinationWHId: destinationWH}
            }else if(destinationWH==undefined){
                 query ={startWHId: startWH}
            }
            else{
                query = {startWIdH:startWH,destinationWHId:destinationWH}
           
            } 
            
            if(startWH==undefined && destinationWH==undefined){
                pathDocument = await this.pathSchema.find();
            }  else{
                
                pathDocument= await this.pathSchema.find(query as FilterQuery<IPathPersistance & Document>)}
                
            
             let paths: Path[]= [];
            pathDocument.forEach(path=>{
                paths.push(PathMap.toDomain(path));
            });
            return paths;
        }

}
