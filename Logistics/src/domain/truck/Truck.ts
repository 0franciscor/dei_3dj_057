import { AggregateRoot } from "../../core/domain/AggregateRoot";
import { UniqueEntityID } from "../../core/domain/UniqueEntityID";
import { Result } from "../../core/logic/Result";
import { ITruckDTO } from "../../dto/ITruckDTO";
import { Autonomy } from "./Autonomy";
import { Capacity } from "./Capacity";
import { FastChargeTime } from "./FastChargeTime";
import { MaxBatteryCapacity } from "./MaxBatteryCapacity";
import { Tare } from "./Tare";
import { TruckID } from "./TruckID";


interface TruckProps {
  tare: Tare;
  capacity: Capacity;
  maxBatteryCapacity: MaxBatteryCapacity;
  autonomy: Autonomy;
  fastChargeTime: FastChargeTime;

}

export class Truck extends AggregateRoot<TruckProps> {
  get id (): UniqueEntityID {
    return this._id;
  }

  get truckID (): TruckID {
    return TruckID.caller(this.id)
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
  



  private constructor (props: TruckProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create (truckDTO:ITruckDTO , id?: UniqueEntityID): Result<Truck> {

    const tare = Tare.create(truckDTO.tare);
    const capacity = Capacity.create(truckDTO.capacity);
    const maxBatteryCapacity = MaxBatteryCapacity.create(truckDTO.maxBatteryCapacity);
    const autonomy = Autonomy.create(truckDTO.autonomy);
    const fastChargeTime = FastChargeTime.create(truckDTO.fastChargeTime);

    const truckPropsResult = Result.combine([tare, capacity, maxBatteryCapacity, autonomy, fastChargeTime]);

    if (truckPropsResult.isFailure) {
      return Result.fail<Truck>(truckPropsResult.error);
    } else {
      const truckProps: TruckProps = {
        tare: tare.getValue(),
        capacity: capacity.getValue(),
        maxBatteryCapacity: maxBatteryCapacity.getValue(),
        autonomy: autonomy.getValue(),
        fastChargeTime: fastChargeTime.getValue(),
      };

      return Result.ok<Truck>(new Truck(truckProps));
    }

  }



}