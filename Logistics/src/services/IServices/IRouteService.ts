import { Result } from "../../core/logic/Result";
import { IRouteDTO } from "../../dto/IRouteDTO";


export default interface IRouteService {

   exist(routeID: string): Promise<Result<boolean>>;
   createRoute(route: IRouteDTO): Promise<Result<IRouteDTO>>;
   getRoute(routeID: string): Promise<Result<IRouteDTO>>;
   getAllRoutes(): Promise<Result<IRouteDTO[]>>;
   updateRoute(route: IRouteDTO): Promise<Result<IRouteDTO>>;
   deleteRoute(routeID: string): Promise<Result<IRouteDTO>>;
}