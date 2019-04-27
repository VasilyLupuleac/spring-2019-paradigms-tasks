#ifndef TSQUEUE_H_
#define TSQUEUE_H_

#include <pthread.h>
#include "queue.h"

extern "C" {

/**
 * Opaque-структура, описывающая потокобезопасную очередь: внутренности нельзя
 * использовать напрямую, только при помощи функцией threadsafe_queue_*().
 */

struct ThreadsafeQueue {
    Queue q;
    pthread_mutex_t m;
    pthread_cond_t cond_element_pushed;
};

/**
 * Инициализирует потокобезопасную очередь, лежащую по указателю `q`.
 * Если очередь по указателю `q` уже инициализирована, поведение не определено.
 * Очередь должна быть освобождена при помощи `threadsafe_queue_destroy`,
 * в противном случае возможны утечки ресурсов.
 */
void threadsafe_queue_init(ThreadsafeQueue *q);

/**
 * Удаляет потокобезопасную очередь, лежащую по указателю `q`, и освобождает
 * все связанные с ней ресурсы. Если очередь непуста,
 * поведение неопределено, так как в ней ещё могут лежат
 * произвольные ресурсы, которые очередь не в состоянии освободить.
 *
 * После удаления все операции с очередью, кроме `threadsafe_queue_init`,
 * неопределены.
 */
void threadsafe_queue_destroy(ThreadsafeQueue *q);

/**
 * Добавляет в очередь произвольный указатель `data`.
 * Эта операция может безопасно вызываться из нескольких потоков одновременно.
 */
void threadsafe_queue_push(ThreadsafeQueue *q, void *data);

/**
 * Удаляет из очереди очередной элемент и возвращает его значение.
 * Если очередь пуста, блокирует выполнение текущего потока до тех пор,
 * пока элемент не будет добавлен при помощи `threadsafe_queue_push()`.
 * Эта операция может безопасно вызываться из нескольких потоков одновременно.
 */
void *threadsafe_queue_wait_and_pop(ThreadsafeQueue *q);
}

#endif
