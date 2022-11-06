using EletricGo.Controllers;
using EletricGo.Domain.Deliveries;
using EletricGo.Domain.Shared;
using Microsoft.AspNetCore.Mvc;
using Moq;
using System.Runtime.Intrinsics.X86;

namespace WarehouseManagementTest.Domain.Deliveries
{
    [TestFixture]
    internal class DeliveryServiceTest
    {
        private readonly string? id = "testID";

        private readonly DateTime deliveryDate = new DateTime(2024, 12, 12);

        private readonly float loadTime = 10;

        private readonly float unloadTime = 20;

        private readonly string? destination = "testDestination";

        private readonly float deliveryMass = 30;

        [Test]
        public void DefineDriverServiceConstrutor()
        {
            var mockDeliveryRepo = new Mock<IDeliveryRepository>();
            var mockUnitWork = new Mock<IUnitOfWork>();

            var service = new DeliveryService(mockUnitWork.Object, mockDeliveryRepo.Object);

            Assert.That(service, Is.Not.Null);
        }

          
        [Test]
        public async Task getDeliveriesTest()
        {
            var mockRepository = new Mock<IDeliveryRepository>();
            mockRepository.Setup(repo => repo.GetAll()).ReturnsAsync(createdDeliveries());
            
            var mockUnit = new Mock<IUnitOfWork>();

            var service = new DeliveryService(mockUnit.Object, mockRepository.Object);

            var expectedList = await service.GetDeliveries();

            var resultList = createdDeliveries();

            Assert.That(resultList, Has.Count.EqualTo(expectedList.Count));
        }

        private List<Delivery> createdDeliveries()
        {
            var deliveryList = new List<Delivery>();
            deliveryList.Add(new Delivery(id, new DeliveryDate(new DateTime(2025, 12, 13)), new LoadTime(1), new UnloadTime(2), new Destination("entregaTeste2"), new DeliveryMass(3)));

            return deliveryList;
        }

        [Test]
        public async Task GetDeliveryTest()
        {
            var deliveryExpected = new Delivery(id, new DeliveryDate(deliveryDate), new LoadTime(loadTime), new UnloadTime(unloadTime), new Destination(destination), new DeliveryMass(deliveryMass));

            var mockRepository = new Mock<IDeliveryRepository>();
            mockRepository.Setup(repo => repo.GetByID(new DeliveryID(this.id))).ReturnsAsync(deliveryExpected);

            var mockUnit = new Mock<IUnitOfWork>();

            var service = new DeliveryService(mockUnit.Object, mockRepository.Object);

            var deliveryResult = await service.GetDelivery(new DeliveryID(this.id));

            Assert.Multiple(() =>
            {
                Assert.That(deliveryResult.deliveryID, Is.EqualTo(deliveryExpected.Id));
                Assert.That(deliveryResult.deliveryDate, Is.EqualTo(deliveryExpected.deliveryDate.date));
                Assert.That(deliveryResult.loadTime, Is.EqualTo(deliveryExpected.loadTime.time));
                Assert.That(deliveryResult.unloadTime, Is.EqualTo(deliveryExpected.unloadTime.time));
                Assert.That(deliveryResult.destination, Is.EqualTo(deliveryExpected.destination.destination));
                Assert.That(deliveryResult.deliveryMass, Is.EqualTo(deliveryExpected.deliveryMass.mass));

            });
        }

        [Test]
        public async Task GetByPeriodTest()
        {
            var delivery = new Delivery(id, new DeliveryDate(deliveryDate), new LoadTime(loadTime), new UnloadTime(unloadTime), new Destination(destination), new DeliveryMass(deliveryMass));
            var dateTime1 = new DateTime(2023, 12, 10);
            var dateTime2 = new DateTime(2023, 12, 14);

            var listExpected = new List<Delivery>() { delivery };

            var mockUnit = new Mock<IUnitOfWork>();
            var mockRepository = new Mock<IDeliveryRepository>();
            mockRepository.Setup(repo => repo.GetByPeriod(dateTime1, dateTime2)).ReturnsAsync(listExpected);

            var service = new DeliveryService(mockUnit.Object, mockRepository.Object);

            var aux = service.GetByPeriod(dateTime1, dateTime2);

            if (aux == null)
                Assert.Fail();
            else
            {
                var listResult = (aux.Result).ToList();
                Assert.AreEqual(listExpected.First().toDeliveryDTO().deliveryID, listResult.First().deliveryID);
            }   
        }

        [Test]
        public async Task CreateDeliveryTest()
        {
            var deliveryExpected = new Delivery(id, new DeliveryDate(deliveryDate), new LoadTime(loadTime), new UnloadTime(unloadTime), new Destination(destination), new DeliveryMass(deliveryMass));

            var deliveryDto = new DeliveryDTO
            {
                deliveryID = this.id,
                deliveryDate = this.deliveryDate,
                loadTime = this.loadTime,
                unloadTime = this.unloadTime,
                destination = this.destination,
                deliveryMass = this.deliveryMass
            };

            var mockRepository = new Mock<IDeliveryRepository>();
            var mockUnit = new Mock<IUnitOfWork>();
            mockRepository.Setup(repo => repo.Add(deliveryExpected));
            mockUnit.Setup(repo => repo.CommitAsync());
            
            mockRepository.Setup(repo => repo.GetByID(new DeliveryID(id))).ReturnsAsync(null as Delivery);

            var service = new DeliveryService(mockUnit.Object, mockRepository.Object);

            var deliveryResult = await service.CreateDelivery(deliveryDto);
            
            Assert.Multiple(() =>
            {
                Assert.That(deliveryResult.deliveryID, Is.EqualTo(deliveryExpected.Id));
                Assert.That(deliveryResult.deliveryDate, Is.EqualTo(deliveryExpected.deliveryDate.date));
                Assert.That(deliveryResult.loadTime, Is.EqualTo(deliveryExpected.loadTime.time));
                Assert.That(deliveryResult.unloadTime, Is.EqualTo(deliveryExpected.unloadTime.time));
                Assert.That(deliveryResult.destination, Is.EqualTo(deliveryExpected.destination.destination));
                Assert.That(deliveryResult.deliveryMass, Is.EqualTo(deliveryExpected.deliveryMass.mass));

            });
        }

        [Test]
        public async Task UpdateDeliveryTest()
        { 
            var deliveryID = new DeliveryID(id);

            var deliveryDto = new DeliveryDTO
            {
                deliveryID = this.id,
                deliveryDate = this.deliveryDate,
                loadTime = this.loadTime,
                unloadTime = 50,
                destination = this.destination,
                deliveryMass = this.deliveryMass
            };

            var deliveryExpected = new Delivery(id, new DeliveryDate(deliveryDate), new LoadTime(loadTime), new UnloadTime(unloadTime), new Destination(destination), new DeliveryMass(deliveryMass));

            var mockRepository = new Mock<IDeliveryRepository>();
            mockRepository.Setup(repo => repo.GetByID(new DeliveryID(id))).ReturnsAsync(deliveryExpected);
            
            var mockUnit = new Mock<IUnitOfWork>();

            var service = new DeliveryService(mockUnit.Object, mockRepository.Object);

            var getDel = await service.UpdateDelivery(deliveryDto);

            Assert.That(deliveryExpected.deliveryMass.mass, Is.EqualTo(deliveryDto.deliveryMass));
        }
        
        
        [Test]
        public async Task DeleteAsyncTest()
        {
            var deliveryID = new DeliveryID(id);

            var deliveryExpected = new Delivery(id, new DeliveryDate(deliveryDate), new LoadTime(loadTime), new UnloadTime(unloadTime), new Destination(destination), new DeliveryMass(deliveryMass));

            var mockRepository = new Mock<IDeliveryRepository>();
            mockRepository.Setup(repo => repo.GetByID(deliveryID)).ReturnsAsync(deliveryExpected);
            mockRepository.Setup(repo => repo.Delete(deliveryExpected));
            
            var mockUnit = new Mock<IUnitOfWork>();

            var service = new DeliveryService(mockUnit.Object, mockRepository.Object);

            var deliveryResult = await service.DeleteDelivery(new DeliveryID(id));

            Assert.Multiple(() =>
            {
                Assert.That(deliveryResult.deliveryID, Is.EqualTo(deliveryExpected.Id));
                Assert.That(deliveryResult.deliveryDate, Is.EqualTo(deliveryExpected.deliveryDate.date));
                Assert.That(deliveryResult.loadTime, Is.EqualTo(deliveryExpected.loadTime.time));
                Assert.That(deliveryResult.unloadTime, Is.EqualTo(deliveryExpected.unloadTime.time));
                Assert.That(deliveryResult.destination, Is.EqualTo(deliveryExpected.destination.destination));
                Assert.That(deliveryResult.deliveryMass, Is.EqualTo(deliveryExpected.deliveryMass.mass));

            });

        }

    }
}