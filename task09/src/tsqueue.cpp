#include "tsqueue.h"

void threadsafe_queue_init(ThreadsafeQueue *q) {
    queue_init(&q->q);
    pthread_mutex_init(&q->m, nullptr);
    pthread_cond_element_pushed_init(&q->cond_element_pushed, nullptr);
}

void threadsafe_queue_destroy(ThreadsafeQueue *q) {
    pthread_cond_element_pushed_destroy(&q->cond_element_pushed);
    pthread_mutex_destroy(&q->m);
    queue_destroy(&q->q);
}

void threadsafe_queue_push(ThreadsafeQueue *q, void *data) {
    pthread_mutex_lock(&q->m);
    queue_push(&q->q, data);
    pthread_cond_element_pushed_signal(&q->cond_element_pushed);
    pthread_mutex_unlock(&q->m);
}

void *threadsafe_queue_wait_and_pop(ThreadsafeQueue *q) {
    pthread_mutex_lock(&q->m);
    while (queue_empty(&q->q))
        pthread_cond_element_pushed_wait(&q->cond_element_pushed, &q->m);
    void *data = queue_pop(&q->q);
    pthread_mutex_unlock(&q->m);
    return data;
}
