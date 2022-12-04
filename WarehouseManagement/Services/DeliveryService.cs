using EletricGo.Domain.Deliveries;
using EletricGo.Domain.Shared;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace EletricGo.Services
{
    public class DeliveryService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IDeliveryRepository _deliveryRepository;

        public DeliveryService(IUnitOfWork unitOfWork, IDeliveryRepository deliveryRepository)
        {
            _unitOfWork = unitOfWork;
            _deliveryRepository = deliveryRepository;
        }

        public virtual async Task<List<DeliveryDTO>> GetDeliveries()
        {
            var deliveries = await _deliveryRepository.GetAll();
            return deliveries.Select(x => x.toDeliveryDTO()).ToList();
        }

        public virtual async Task<List<DeliveryPrologDTO>> GetDeliveriesProlog()
        {
            var deliveries = await _deliveryRepository.GetAll();
            return deliveries.Select(x => x.toDeliveryDTOProlog()).ToList();
        }

        public virtual async Task<DeliveryDTO> GetDelivery(DeliveryID deliveryID)
        {
            var delivery = await _deliveryRepository.GetByID(deliveryID);

            if (delivery == null)
                return null;

            return delivery.toDeliveryDTO();
        }

        public virtual async Task<List<DeliveryDTO>> GetByPeriod(DateTime date1, DateTime date2)
        {
            var deliveries = await _deliveryRepository.GetByPeriod(date1, date2);

            if (deliveries == null)
                return null;

            return deliveries.Select(x => x.toDeliveryDTO()).ToList();
        }

        public virtual async Task<DeliveryDTO> CreateDelivery(DeliveryDTO deliveryDTO)
        {
            if (await _deliveryRepository.GetByID(new DeliveryID(deliveryDTO.deliveryID)) != null)
                return null;

            var delivery = new Delivery(deliveryDTO);

            if (delivery == null)
                return null;

            try{
                await _deliveryRepository.Add(delivery);
            } catch (Exception)
            {
                return null;
            }
            await _unitOfWork.CommitAsync();
            return delivery.toDeliveryDTO();
        }

        public virtual async Task<DeliveryDTO> UpdateDelivery(DeliveryDTO deliveryDTO)
        {
            var delivery = await _deliveryRepository.GetByID(new DeliveryID(deliveryDTO.deliveryID));

            if (delivery == null)
                return null;

            delivery.Update(deliveryDTO);
            await _unitOfWork.CommitAsync();
            return delivery.toDeliveryDTO();
        }

        public virtual async Task<DeliveryDTO> DeleteDelivery(DeliveryID deliveryID)
        {
            var delivery = await _deliveryRepository.GetByID(deliveryID);

            if (delivery == null)
                return null;

            _deliveryRepository.Delete(delivery);
            await _unitOfWork.CommitAsync();
            return delivery.toDeliveryDTO();
        }

        public async Task<bool> FindDelivery(DeliveryID deliveryID)
        {
            return await _deliveryRepository.Find(deliveryID);
        }
    }
}