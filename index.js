const fetch = require('node-fetch')
const cheerio = require('cheerio')
const moment = require('moment')
const { groupBy, sum } = require('lodash')

fetchShootings()
  .then(shootings => {
    const sep = '\t'
    console.log(columns.join(sep))
    shootings.forEach(s => {
      const values = columns.map(col => {
        const val = s[col]
        return typeof val === 'string' && needsEscaping(val)
          ? `"${escapeChars(val)}"`
          : val
      })
      console.log(values.join(sep))
    })
  })
  .catch(err => console.error(err))

function needsEscaping (val) {
  return val.includes('\n') || val.includes('"') || val.includes(',')
}

function escapeChars (val) {
  return val.replace(/"/g, '""')
}

function fetchShootings () {
  return fetch('https://en.wikipedia.org/wiki/List_of_school_shootings_in_the_United_States')
    .then(res => res.text())
    .then(body => parse(body))
}

function parse (body) {
  const $ = cheerio.load(body)

  return $('.wikitable > tbody tr')
    .filter((index, tr) =>
      // ignore headings
      $(tr).children().first().text() !== 'Date'
    )
    .map((index, tr) =>
      $(tr)
        .children()
        .map((i, td) =>
          $(td).text()
        )
        .toArray()
        .reduce((res, val, index) => {
          res[indexToColumn(index)] = val
          return res
        }, {})
    )
    .toArray()
    .map(({ date, location, deaths, injuries, description }) => {
      const intInjuries = parseInt(injuries)
      return {
        date: moment(
          date.startsWith('00000000') ? date.slice(23) : date,
          'MMMM DD, YYYY'
        ).toISOString(),
        deaths: parseInt(deaths),
        // it's probably a '?'. only occurs a few times.
        injuries: Number.isNaN(intInjuries) ? 0 : intInjuries,
        location: location.includes('!') ? location.split('!')[1] : location,
        description
      }
    })
}

const columns = [
  'date',
  'location',
  'deaths',
  'injuries',
  'description'
]
function indexToColumn (i) {
  return columns[i]
}
