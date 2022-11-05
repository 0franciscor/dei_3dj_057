import { Result } from "../../core/logic/Result";
import { IRoutesDTO } from "../../dto/IRoutesDTO";


export default interface IRoutesService {

   exist(routeID: string): Promise<Result<boolean>>;
   createRoute(route: IRoutesDTO): Promise<Result<IRoutesDTO>>;
   getRoutes(routeID: string): Promise<Result<IRoutesDTO>>;
   getAllRoute(): Promise<Result<IRoutesDTO[]>>;
   updateRoute(route: IRoutesDTO): Promise<Result<IRoutesDTO>>;
   deleteRoute(routeID: string): Promise<Result<IRoutesDTO>>;
}