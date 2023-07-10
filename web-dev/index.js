let score = 0;
let questionIndex = 0;
const TOTAL_QUESTIONS = 10;


const updateQuestion = (triviaQuestion) => {

    if (questionIndex !== 0) {
        document.querySelector('.correctAnswer').className = 'incorrectAnswer';
    }
    const randomIndex = Math.floor(Math.random() * 4);

    document.querySelectorAll('.incorrectAnswer')[randomIndex].className = 'correctAnswer';

    document.querySelector('.question').innerText = triviaQuestion.question.text;
    triviaQuestion.incorrectAnswers.forEach((answer, index) => {
        document.querySelectorAll('.incorrectAnswer')[index].innerText = answer;
    });
    document.querySelector('.correctAnswer').innerText = triviaQuestion.correctAnswer;
    questionIndex++;

}

window.onload = async () => {
    const response = await fetch('https://the-trivia-api.com/v2/questions');
    const data = await response.json();


    const triviaQuestions = data.map(triviaData => {
        const { category, correctAnswer, incorrectAnswers, question} = triviaData;
        return { category, correctAnswer, incorrectAnswers, question };
    });

    updateQuestion(triviaQuestions[questionIndex]);

    document.querySelectorAll('button').forEach(button => {
        button.addEventListener('click', () => {
            if (button.classList.contains('correctAnswer')) {
                score++;
            }
            document.querySelector('.score').innerText = `Score: ${score}/${questionIndex}`;

            updateQuestion(triviaQuestions[questionIndex]);
        });
    });
};