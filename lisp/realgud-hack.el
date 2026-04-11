;;; realgud-hack.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2026-04-11 16:45]

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; For some reason, the comint filter will occasionally be called
;; twice. In my experimentation, I have been able to consistently
;; reproduce this with the following conditions:
;;
;; - I am running a debugger commands that takes longer
;;   - In my testing, I was using `rr` with the `reverse-next`
;;     command. However, on some machines that I've used, `gdb`s
;;     `next` command is SOMETIMES slow enough to trigger this
;;     behavior, which is INFURIATING.
;; - the command buffer is displayed in one of the other windows
;;
;; Why is it these circumstances exactly? I'm not sure. It seems
;; strange that this doesn't happen when the command buffer isn't
;; shown, but maybe that's a hint?
;;
;; Anyway, the writers seem to be aware of this based on the comments
;; in the function. However, it's NOT true that nothing changes the
;; second time.
;;
;; The first time the comint filter is called, we get down to the
;; following stack trace:
;;
;; - realgud-track-comint-output-filter-hook
;;   - realgud:track-from-region
;;     - realgud-track-loc-action
;;       - realgud-cmdbuf-info-in-srcbuf?
;;       - (realgud-cmdbuf-info-in-srcbuf?= nil)
;;
;; realgud-track-loc-action will first check if we need to return to
;; the srcbuf, which is yes. Then it sets in-srcbuf? to nil, which is
;; fine.
;;
;; Except its NOT fine. When the second time the output filter gets
;; called, NOW in-srcbuf? is nil, and the cmdbuf grabs the cursor.
;;
;; It was my expectation that the cursor should remain in the srcbuf,
;; but this is not properly handled in the case where the
;; output-filter gets called twice, which again doesn't happen every time.
;;
;; So, what are we to do?
;;
;; - If we could prevent two calls from happening, this would be the
;;   cleanest. However, I'm not sure this is possible.
;; - If we could detect that the second call is happening, we could
;;   restore the previous value of in-srcbuf? before running
;;   realgud:track-from-region a second time.
;;
;; This is my hacky way of detecting that a second run is
;; happening. I've noticed that both during both calls, the value of
;; `last-output-start` is the same. So, I can detect that the second
;; call is happening by recording the value of `last-output-start` in
;; the first call, and checking - in the second call - the value is
;; the same. I don't think the restore code is fully correct, but it's
;; a hack for now until I can investigate a better solution that's
;; worth upstreaming.
;;
;; IMO there should be a clean way to handle this without needing to
;; employ a global. One way I can think of is that
;; realgud:track-from-region should be able to see that during the
;; first call, the "(gdb)" (or in my case "(rr)") prompt hasn't been
;; fully printed yet, and thus it should inform
;; realgud-track-loc-action that it sohuldn't clear `in-srcbuf?`
;; *yet*. Then, when the second time runs, we see the prompt in the
;; cmdbuf, and we can safely clear `in-srcbuf?`.
;;
;; In fact, the comment even points to there being some dbgr prompt
;; parsing, but I can't find it!
;;
;; However, that fix seems a bit comprehensive. Now all debugger types
;; will need to register a regexp for what their prompt looks like. Is
;; there really not a better way?

;;; Code:

(defvar realgud-track-previous-output-start nil)

(defun my/realgud-track-comint-output-filter-hook(text)
  "An output-filter hook custom for comint shells.  Find
location/s, if any, and run the action(s) associated with
finding a new location/s.  The parameter TEXT appears because it
is part of the comint-output-filter-functions API. Instead we use
marks set in buffer-local variables to extract text"

  ;; Instead of trying to piece things together from partial text
  ;; (which can be almost useless depending on Emacs version), we
  ;; monitor to the point where we have the next dbgr prompt, and then
  ;; check all text from comint-last-input-end to process-mark.

  ;; FIXME: Add unwind-protect?
  (if (and realgud-track-mode (realgud-cmdbuf? (current-buffer)))
      (let* ((cmd-buff (current-buffer))
	     (cmd-mark (point-marker))
	     (shortkey
	      (realgud-cmdbuf-info-src-shortkey?
	       realgud-cmdbuf-info))
	     (curr-proc (get-buffer-process cmd-buff))
	     (cmdbuf-last-output-end
	      (realgud-cmdbuf-info-last-input-end realgud-cmdbuf-info))
	     (last-output-end
	      (if curr-proc
		  (process-mark curr-proc)
		cmdbuf-last-output-end))
	     (last-output-start (max comint-last-input-start
				     (- last-output-end realgud-track-char-range))))
	;; Sometimes we get called twice and the second time nothing
	;; changes. Guard against this.
	(unless (= last-output-start last-output-end)
	  (unless (= last-output-end cmdbuf-last-output-end)
	    (setq last-output-start (max last-output-start
					 cmdbuf-last-output-end))
	    )
          (when (and realgud-track-previous-output-start
                     (= last-output-start realgud-track-previous-output-start))
            (realgud-cmdbuf-info-in-srcbuf?= t))
          (setq realgud-track-previous-output-start last-output-start)
	  ;; Done with using old command buffer's last-input-end.
	  ;; Update that for next time.
	  (realgud-cmdbuf-info-last-input-end= last-output-start)
	  (realgud:track-from-region last-output-start
				     last-output-end cmd-mark cmd-buff
				     shortkey 't))
	)
    )
  )

(advice-add #'realgud-track-comint-output-filter-hook
            :override
            #'my/realgud-track-comint-output-filter-hook)

(provide 'realgud-hack)
;;; realgud-hack.el ends here
