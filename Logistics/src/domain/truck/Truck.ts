import { AggregateRoot } from "../../core/domain/AggregateRoot";
import { UniqueEntityID } from "../../core/domain/UniqueEntityID";
import { Result } from "../../core/logic/Result";
import { ITruckDTO } from "../../dto/ITruckDTO";
import { Active } from "./Active";
import { Autonomy } from "./Autonomy";
import { Capacity } from "./Capacity";
import { FastChargeTime } from "./FastChargeTime";
import { MaxBatteryCapacity } from "./MaxBatteryCapacity";
import { Tare } from "./Tare";
import { TruckID } from "./TruckID";


interface TruckProps {
  truckID: TruckID;
  tare: Tare;
  capacity: Capacity;
  maxBatteryCapacity: MaxBatteryCapacity;
  autonomy: Autonomy;
  fastChargeTime: FastChargeTime;
  active: Active;
}

export class Truck extends AggregateRoot<TruckProps> {
  get id (): UniqueEntityID {
    return this._id;
  }

  get truckID (): TruckID {
    return this.props.truckID;
  }

  get capacity (): Capacity {
    return this.props.capacity;
  }

  get maxBatteryCapacity (): MaxBatteryCapacity {
    return this.props.maxBatteryCapacity;
  }

  get autonomy (): Autonomy {
    return this.props.autonomy;
  }

  get fastChargeTime (): FastChargeTime {
    return this.props.fastChargeTime;
  }

  get tare (): Tare {
    return this.props.tare;
  }

  get active (): Active {
    return this.props.active;
  }
  

  set tare (tare: Tare) {
    this.props.tare = tare;
  }

  set capacity (capacity: Capacity) {
    this.props.capacity = capacity;
  }

  set maxBatteryCapacity (maxBatteryCapacity: MaxBatteryCapacity) {
    this.props.maxBatteryCapacity = maxBatteryCapacity;
  }

  set autonomy (autonomy: Autonomy) {
    this.props.autonomy = autonomy;
  }

  set fastChargeTime (fastChargeTime: FastChargeTime) {
    this.props.fastChargeTime = fastChargeTime;
  }

  set active (active: Active) {
    this.props.active = active;
  }
  



  private constructor (props: TruckProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create (truckDTO:ITruckDTO , id?: UniqueEntityID): Result<Truck> {

    try {
      const truck = new Truck({
        truckID: TruckID.create(truckDTO.truckID).getValue(),
        tare:Tare.create(truckDTO.tare).getValue(),
        capacity:Capacity.create(truckDTO.capacity).getValue(),
        maxBatteryCapacity:MaxBatteryCapacity.create(truckDTO.maxBatteryCapacity).getValue(),
        autonomy:Autonomy.create(truckDTO.autonomy).getValue(),
        fastChargeTime:FastChargeTime.create(truckDTO.fastChargeTime).getValue(),
        active:Active.create(truckDTO.active).getValue()
      }, id);
  
      return Result.ok<Truck>(truck);

    } catch (error) {
      return Result.fail<Truck>(error);
    }
      
}
      
  



}