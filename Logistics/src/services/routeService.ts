
import { Inject, Service } from "typedi";
import { Result } from "../core/logic/Result";
import { Route } from "../domain/route/Route";
import { Date } from "../domain/route/Date";
import { IRouteDTO } from "../dto/IRouteDTO";
import { RouteMap } from "../mappers/RouteMap";
import IRouteRepo from "./IRepos/IRouteRepo";
import IRouteService from "./IServices/IRouteService";
import config from "../../config";
import { Warehouses } from "../domain/route/Warehouses";

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
                console.log(routeDTO);
            const routeOrError = Route.create(routeDTO);
            console.log(routeOrError);
            if(routeOrError.isFailure) {
                return Result.fail<IRouteDTO>(routeOrError.error);
            }
            const routeResult = routeOrError.getValue();
            console.log(routeResult);
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
            console.log("get all routes");
            const routes = await this.routeRepo.getAllRoutes();
            console.log(routes)
            const routeDTOResult = RouteMap.toDTOList(routes) as IRouteDTO[];
            return Result.ok<IRouteDTO[]>(routeDTOResult);
        }catch(e){
            throw e;
        }       
    }

    public async updateRoute(routeDTO: IRouteDTO): Promise<Result<IRouteDTO>> {
        try {
            let route = await this.routeRepo.getRouteById(routeDTO.routeID);
            if(route === null)
                return Result.fail<IRouteDTO>("Route not found");

            if(routeDTO.date != route.date.date && routeDTO.date != null)
                route.date = Date.create(routeDTO.date).getValue();
            if(routeDTO.warehouses != route.warehouse.warehouse && routeDTO.warehouses != null)
                route.warehouse = Warehouses.create(routeDTO.warehouses).getValue();

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

