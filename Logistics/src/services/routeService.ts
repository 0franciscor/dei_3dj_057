
import { Inject, Service } from "typedi";
import { Result } from "../core/logic/Result";
import { Route } from "../domain/route/Route";
import { Date } from "../domain/route/Date";
import { IRouteDTO } from "../dto/IRouteDTO";
import { RouteMap } from "../mappers/RouteMap";
import IRouteRepo from "./IRepos/IRouteRepo";
import IRouteService from "./IServices/IRouteService";
import config from "../../config";
import { Packaging } from "../domain/packaging/Packaging";
import { PackagingID } from "../domain/packaging/PackagingID";
import { TruckID } from "../domain/truck/TruckID";


@Service()
export default class RouteService implements IRouteService {
    constructor(
        @Inject(config.repos.route.name) private routeRepo: IRouteRepo,
     ) { }


    public async exist(routeID: string): Promise<Result<boolean>> {
        try {
            const routeResult = await this.routeRepo.getRouteById(routeID);
            if(routeResult === null)
                return Result.ok<boolean>(false);
            return Result.ok<boolean>(true);
        }catch(e){
            throw e;
        }
    }

    public async createRoute(routeDTO: IRouteDTO): Promise<Result<IRouteDTO>> {
        try{
            const route = await this.routeRepo.getRouteById(routeDTO.routeID);
            if(route !== null)
                return Result.fail<IRouteDTO>("Route already exists");
            const routeOrError = Route.create(routeDTO);
            if(routeOrError.isFailure) {
                return Result.fail<IRouteDTO>(routeOrError.error);
            }
            const routeResult = routeOrError.getValue();
            await this.routeRepo.save(routeResult);

            const routeDTOResult = RouteMap.toDTO(routeResult) as IRouteDTO;
            return Result.ok<IRouteDTO>(routeDTOResult);
        }catch(e){
            throw e;
        }
    } 


    public async getRoute(routeID: string): Promise<Result<IRouteDTO>>{
        try{
            
            const route = await this.routeRepo.getRouteById(routeID);
            if(route === null)
                return Result.fail<IRouteDTO>("Route not found");

            const routeDTOResult = RouteMap.toDTO(route) as IRouteDTO;
            return Result.ok<IRouteDTO>(routeDTOResult);

        }catch (e){
            throw e;
        }
    }

    public async getAllRoutes(): Promise<Result<IRouteDTO[]>> {
        try{
            const routes = await this.routeRepo.getAllRoutes();
            const routeDTOResult = RouteMap.toDTOList(routes) as IRouteDTO[];
            return Result.ok<IRouteDTO[]>(routeDTOResult);
        }catch(e){
            throw e;
        }       
    }

    public async updateRoute(routeDTO: IRouteDTO): Promise<Result<IRouteDTO>> {
        try {
            const route = await this.routeRepo.getRouteById(routeDTO.routeID);
            if(route === null)
                return Result.fail<IRouteDTO>("Route not found");

            if(routeDTO.packagingID !== route.packaging.id && routeDTO.packagingID!=null){
                const packagingOrError = PackagingID.create(routeDTO.packagingID);
                if(packagingOrError.isFailure){
                    return Result.fail<IRouteDTO>(packagingOrError.error);
                }

                route.packaging = packagingOrError.getValue();

            }
                
            if(routeDTO.truckID !== route.truck.id && routeDTO.truckID!=null){
                const truckOrError = TruckID.create(routeDTO.truckID);
                if(truckOrError.isFailure){
                    return Result.fail<IRouteDTO>(truckOrError.error);
                }

                route.truck = truckOrError.getValue();
            }
               
            if(routeDTO.date !== route.date.date && routeDTO.date!=null){
                const dateOrError = Date.create(routeDTO.date);
                if(dateOrError.isFailure) {
                    return Result.fail<IRouteDTO>(dateOrError.error);
                }
                route.date = dateOrError.getValue();
            }

            await this.routeRepo.save(route);

            const routeDTOResult = RouteMap.toDTO(route) as IRouteDTO;
            return Result.ok<IRouteDTO>(routeDTOResult);
        }catch(e){
            throw e;
        }

    }

        public async deleteRoute(routeID: string): Promise<Result<IRouteDTO>> {
            try{

                const route = await this.routeRepo.getRouteById(routeID);
                if(route === null)
                    return Result.fail<IRouteDTO>("Route not found");
                   
                await this.routeRepo.delete(route);
                
                const routeDTOResult = RouteMap.toDTO(route) as IRouteDTO;
                return Result.ok<IRouteDTO>(routeDTOResult);
            } catch (e){
                throw e;
            }
        }        
    }   

