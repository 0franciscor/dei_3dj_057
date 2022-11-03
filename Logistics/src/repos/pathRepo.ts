import { Document,FilterQuery,Model } from "mongoose";
import { Inject, Service } from "typedi";
import IPathRepo from './IRepos/IPathRepo'
import { IPathPersistance } from "../dataschema/IPathPersistance";
import { Path } from "../domain/path/Path";
import { PathID } from '../domain/path/PathID';
import { PathMap } from "../mappers/PathMap";


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
    public async delete (path: Path): Promise<Path>{
        const query = {pathId:path.pathID.id};
        const pathDocument = await this.pathSchema.findOne(query as 
        FilterQuery<IPathPersistance & Document>); 
        try {
            if(pathDocument === null){
                return path;
            }
            else{
                await this.pathSchema.deleteOne(query as 
                FilterQuery<IPathPersistance & Document>);
                return path;
            }
        }catch(error){
            throw error;
        }
    }
        
        public async getPathById(pathID: string | PathID): Promise<Path> {
            const query ={pathID: pathID};
            const pathDocument = await this.pathSchema.findOne(query as 
            FilterQuery<IPathPersistance & Document>);

            if(pathDocument === null){
                return null;
            }
            else{
                return PathMap.toDomain(pathDocument);
            }
        }
        
        public async getAllPaths(): Promise<Path[]> {
            const pathDocument = await this.pathSchema.find();

            let paths: Path[]= [];
            pathDocument.forEach(path=>{
                paths.push(PathMap.toDomain(path));
            });
            return paths;
        }

}
