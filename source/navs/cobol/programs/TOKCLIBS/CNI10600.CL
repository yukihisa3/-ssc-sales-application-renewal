/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    実績累積管理                         *  ./
/. *   JOB-ID      :    CNI10600                             *  ./
/. *   JOB-NAME    :    実績累積Ｆ削除                       *  ./
/. *   UPDATE      :    20111110 基幹サーバ統合              *  ./
/. ***********************************************************  ./
    PGM  (P1-?KBN)
/.###ﾊﾟﾗﾒﾀ定義####./
    PARA ?KBN     ,STRING*2,OUT,VALUE-'  ' /.OK,NG./
/.###ﾜｰｸｴﾘｱ定義####./
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'CNI10600'
    VAR       ?STEP     ,STRING*8
    VAR       ?JBNM     ,STRING*24!MIXED            /.業務漢字名 ./
    VAR       ?JBID     ,STRING*10                  /.業務ＩＤ   ./
    VAR       ?PGID     ,STRING*10                  /.ＰＧＩＤ　 ./
    VAR       ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                  /.      2    ./
    VAR       ?OPR3     ,STRING*50                  /.      3    ./
    VAR       ?OPR4     ,STRING*50                  /.      4    ./
    VAR       ?OPR5     ,STRING*50                  /.      5    ./
    VAR       ?PGNM     ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1    ,STRING*40                  /.      2    ./
    VAR       ?KEKA2    ,STRING*40                  /.      3    ./
    VAR       ?KEKA3    ,STRING*40                  /.      4    ./
    VAR       ?KEKA4    ,STRING*40                  /.      5    ./
    VAR       ?OK       ,STRING*2,VALUE-'OK'        /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?NG       ,STRING*2,VALUE-'NG'        /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?FLG      ,STRING*2,VALUE-'  '

/.PG名称ｾｯﾄ./

    ?PGNM  :=  '受払実績ＤＴ－月次更新処理'

/.ﾗｲﾌﾞﾗﾘﾘｽﾄ登録./

    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB

/.20111110 ADD バックアップフラグ　ＯＮ／ＯＦＦ./

    SNDMSG MSG-'バックアップフラグチェック',TO-XCTL
    OVRF FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL JISBKFLG.TOKELIBO,PARA-(?FLG)

  IF ?FLG=  'ON'   THEN
    SNDMSG MSG-'#＃実績関連Ｆ　退避中＃',TO-XCTL
    SETPF FILE-RUISEKB2.TOKKLIB,TOFILE-RUISEKB3.TOKKLIB,ADD-@NO,
          ACTCHK-@NO
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## SETPF(RUISEKB3)  ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績累積Ｆ３コピー】'
              GOTO ABEND END
    SETPF FILE-RUISEKB1.TOKKLIB,TOFILE-RUISEKB2.TOKKLIB,ADD-@NO,
          ACTCHK-@NO
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## SETPF(RUISEKB2)  ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績累積Ｆ２コピー】'
              GOTO ABEND END
    SETPF FILE-RUISEKF.TOKFLIB,TOFILE-RUISEKB1.TOKKLIB,ADD-@NO,
          ACTCHK-@NO
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## SETPF(RUISEKB1)  ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績累積Ｆ１コピー】'
              GOTO ABEND END
    SETPF FILE-JISSYUB2.TOKKLIB,TOFILE-JISSYUB3.TOKKLIB,ADD-@NO,
          ACTCHK-@NO
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## SETPF(JISSYUB3)  ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績集計Ｆ３コピー】'
              GOTO ABEND END
    SETPF FILE-JISSYUB1.TOKKLIB,TOFILE-JISSYUB2.TOKKLIB,ADD-@NO,
          ACTCHK-@NO
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## SETPF(JISSYUB2)  ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績集計Ｆ２コピー】'
              GOTO ABEND END
    SETPF FILE-JISSYUF.TOKFLIB,TOFILE-JISSYUB1.TOKKLIB,ADD-@NO,
          ACTCHK-@NO
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## SETPF(JISSYUB1)  ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績集計Ｆ１コピー】'
              GOTO ABEND END
    DACTLF FILE-JISSSB11.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## DACTLF(JISSSB11) ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績前年週集計　ＤＡＣＴ１】'
              GOTO ABEND END
    DACTLF FILE-JISSSB12.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## DACTLF(JISSSB12) ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績前年週集計　ＤＡＣＴ２】'
              GOTO ABEND END
    DACTLF FILE-JISSSB21.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## DACTLF(JISSSB21) ERR ##',TO-XCTL
              ?KEKA4  :=  '【２年前週集計　ＤＡＣＴ１】'
              GOTO ABEND END
    DACTLF FILE-JISSSB22.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## DACTLF(JISSSB22) ERR ##',TO-XCTL
              ?KEKA4  :=  '【２年前週集計　ＤＡＣＴ２】'
              GOTO ABEND END
    DACTLF FILE-JISSSB31.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## DACTLF(JISSSB31) ERR ##',TO-XCTL
              ?KEKA4  :=  '【３年前週集計　ＤＡＣＴ１】'
              GOTO ABEND END
    DACTLF FILE-JISSSB32.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## DACTLF(JISSSB32) ERR ##',TO-XCTL
              ?KEKA4  :=  '【３年前週集計　ＤＡＣＴ２】'
              GOTO ABEND END
    SETPF FILE-JISSSB2.TOKKLIB,TOFILE-JISSSB3.TOKKLIB,ADD-@NO,
          ACTCHK-@NO
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## SETPF(JISSSB3)  ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績週集計Ｆ３コピー】'
              GOTO ABEND END
    SETPF FILE-JISSSB1.TOKKLIB,TOFILE-JISSSB2.TOKKLIB,ADD-@NO,
          ACTCHK-@NO
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## SETPF(JISSSB2)  ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績週集計Ｆ２コピー】'
              GOTO ABEND END
    SETPF FILE-JISSSYUF.TOKKLIB,TOFILE-JISSSB1.TOKKLIB,ADD-@NO,
          ACTCHK-@NO
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## SETPF(JISSSB1)  ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績週集計Ｆ１コピー】'
              GOTO ABEND END
    ACTLF FILE-JISSSB11.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## ACTLF(JISSSB11) ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績前年週集計　ＡＣＴ１】'
              GOTO ABEND END
    ACTLF FILE-JISSSB12.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## ACTLF(JISSSB12) ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績前年週集計　ＡＣＴ２】'
              GOTO ABEND END
    ACTLF FILE-JISSSB21.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## ACTLF(JISSSB21) ERR ##',TO-XCTL
              ?KEKA4  :=  '【２年前週集計　ＡＣＴ１】'
              GOTO ABEND END
    ACTLF FILE-JISSSB22.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## ACTLF(JISSSB22) ERR ##',TO-XCTL
              ?KEKA4  :=  '【２年前週集計　ＡＣＴ２】'
              GOTO ABEND END
    ACTLF FILE-JISSSB31.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## ACTLF(JISSSB31) ERR ##',TO-XCTL
              ?KEKA4  :=  '【３年前週集計　ＡＣＴ１】'
              GOTO ABEND END
    ACTLF FILE-JISSSB32.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## ACTLF(JISSSB32) ERR ##',TO-XCTL
              ?KEKA4  :=  '【３年前週集計　ＡＣＴ２】'
              GOTO ABEND END
  END

/.実績累積Ｆ保有期間外データ削除./
SJS0110B:

    ?STEP :=   'SJS0110B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'#＃実績累積Ｆ　削除中＃',TO-XCTL

    OVRF FILE-GERUISKF,TOFILE-GERUISKF.TOKBLIB
    OVRF FILE-RUISEKL4,TOFILE-RUISEKL4.TOKFLIB
    OVRF FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL  SJS0110B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【実績累積Ｆ　削除処理】'
              GOTO ABEND END

/.実績累積Ｆ保有期間外データ削除./
PCNDPF:

    ?STEP :=   'PCNDPF  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'#＃実績累積Ｆ　圧縮中＃',TO-XCTL

    DACTLF FILE-RUISEKL1.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## DACTLF(RUISEKL1) ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績累積Ｆ　ＤＡＣＴ１】'
              GOTO ABEND END
    DACTLF FILE-RUISEKL2.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## DACTLF(RUISEKL2) ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績累積Ｆ　ＤＡＣＴ２】'
              GOTO ABEND END
    DACTLF FILE-RUISEKL3.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## DACTLF(RUISEKL3) ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績累積Ｆ　ＤＡＣＴ３】'
              GOTO ABEND END
    DACTLF FILE-RUISEKL4.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## DACTLF(RUISEKL4) ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績累積Ｆ　ＤＡＣＴ４】'
              GOTO ABEND END
    DACTLF FILE-RUISEKL5.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## DACTLF(RUISEKL5) ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績累積Ｆ　ＤＡＣＴ５】'
              GOTO ABEND END
    DACTLF FILE-RUISEKLA.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## DACTLF(RUISEKLA) ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績累積Ｆ　ＤＡＣＴＡ】'
              GOTO ABEND END
    DACTLF FILE-RUISEKL6.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## DACTLF(RUISEKL6) ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績累積Ｆ　ＤＡＣＴ６】'
              GOTO ABEND END
    CNDPF FILE-RUISEKF.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## CNDPF(RUISEKFG)  ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績累積Ｆ　圧縮中】'
              GOTO ABEND END
    ACTLF FILE-RUISEKL1.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## ACTLF(RUISEKL1)  ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績累積Ｆ　ＡＣＴ１】'
              GOTO ABEND END
    ACTLF FILE-RUISEKL2.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## ACTLF(RUISEKL2)  ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績累積Ｆ　ＡＣＴ２】'
              GOTO ABEND END
    ACTLF FILE-RUISEKL3.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## ACTLF(RUISEKL3)  ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績累積Ｆ　ＡＣＴ３】'
              GOTO ABEND END
    ACTLF FILE-RUISEKL4.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## ACTLF(RUISEKL4)  ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績累積Ｆ　ＡＣＴ４】'
              GOTO ABEND END
    ACTLF FILE-RUISEKL5.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## ACTLF(RUISEKL5)  ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績累積Ｆ　ＡＣＴ５】'
              GOTO ABEND END
    ACTLF FILE-RUISEKLA.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## ACTLF(RUISEKLA)  ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績累積Ｆ　ＡＣＴＡ】'
              GOTO ABEND END
    ACTLF FILE-RUISEKL6.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## ACTLF(RUISEKL6)  ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績累積Ｆ　ＡＣＴ６】'
              GOTO ABEND END

/.実績集計Ｆ保有期間外データ削除./
SJS0120B:

    ?STEP :=   'SJS0120B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'#＃実績集計Ｆ　削除中＃',TO-XCTL

    OVRF FILE-JISSYUL4,TOFILE-JISSYUL4.TOKFLIB
    OVRF FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL  SJS0120B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【実績集計Ｆ　削除処理】'
              GOTO ABEND END

/.実績集計Ｆ保有期間外データ削除./
PCNDPF1:

    ?STEP :=   'PCNDPF1 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'#＃実績累積Ｆ　圧縮中＃',TO-XCTL

    DACTLF FILE-JISSYUL1.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## DACTLF(JISSYUL1) ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績集計Ｆ　ＤＡＣＴ１】'
              GOTO ABEND END
    DACTLF FILE-JISSYUL2.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## DACTLF(JISSYUL2) ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績集計Ｆ　ＤＡＣＴ２】'
              GOTO ABEND END
    DACTLF FILE-JISSYUL3.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## DACTLF(JISSYUL3) ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績集計Ｆ　ＤＡＣＴ３】'
              GOTO ABEND END
    DACTLF FILE-JISSYUL4.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## DACTLF(JISSYUL4) ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績集計Ｆ　ＤＡＣＴ４】'
              GOTO ABEND END
    DACTLF FILE-JISSYUL5.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## DACTLF(JISSYUL5) ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績集計Ｆ　ＤＡＣＴ５】'
              GOTO ABEND END
    DACTLF FILE-JISSYUL6.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## DACTLF(JISSYUL6) ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績集計Ｆ　ＤＡＣＴ６】'
              GOTO ABEND END
    CNDPF FILE-JISSYUF.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## CNDPF(JISSYUFG)  ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績集計Ｆ　圧縮中】'
              GOTO ABEND END
    ACTLF FILE-JISSYUL1.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## ACTLF(JISSYUL1)  ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績集計Ｆ　ＡＣＴ１】'
              GOTO ABEND END
    ACTLF FILE-JISSYUL2.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## ACTLF(JISSYUL2)  ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績集計Ｆ　ＡＣＴ２】'
              GOTO ABEND END
    ACTLF FILE-JISSYUL3.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## ACTLF(JISSYUL3)  ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績集計Ｆ　ＡＣＴ３】'
              GOTO ABEND END
    ACTLF FILE-JISSYUL4.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## ACTLF(JISSYUL4)  ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績集計Ｆ　ＡＣＴ４】'
              GOTO ABEND END
    ACTLF FILE-JISSYUL5.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## ACTLF(JISSYUL5)  ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績集計Ｆ　ＡＣＴ５】'
              GOTO ABEND END
    ACTLF FILE-JISSYUL6.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## ACTLF(JISSYUL6)  ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績集計Ｆ　ＡＣＴ６】'
              GOTO ABEND END

/. 20111110-ADD実績週集計Ｆ保有期間外データ削除./
/.実績週集計Ｆ保有期間外データ削除./
SJS0130B:

    ?STEP :=   'SJS0130B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'#＃実績週集計Ｆ　削除中＃',TO-XCTL

    OVRF FILE-JISSSYU4,TOFILE-JISSSYU4.TOKKLIB
    OVRF FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRF FILE-HIDUKEL1,TOFILE-HIDUKEL1.TOKWLIB
    CALL  SJS0130B.TOKELIBO
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【実績週集計Ｆ　削除処理】'
              GOTO ABEND END

PCNDPF2:

    ?STEP :=   'PCNDPF2 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'#＃実績週集計Ｆ　圧縮中＃',TO-XCTL

    DACTLF FILE-JISSSYU1.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## DACTLF(JISSSYU1) ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績週集計Ｆ　ＤＡＣＴ１】'
              GOTO ABEND END
    DACTLF FILE-JISSSYU2.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## DACTLF(JISSSYU2) ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績週集計Ｆ　ＤＡＣＴ２】'
              GOTO ABEND END
    DACTLF FILE-JISSSYU3.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## DACTLF(JISSSYU3) ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績週集計Ｆ　ＤＡＣＴ３】'
              GOTO ABEND END
    DACTLF FILE-JISSSYU4.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## DACTLF(JISSSYU4) ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績週集計Ｆ　ＤＡＣＴ４】'
              GOTO ABEND END
    CNDPF FILE-JISSSYUF.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## CNDPF(JISSSYUF)  ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績集計Ｆ　圧縮中】'
              GOTO ABEND END
    ACTLF FILE-JISSSYU1.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## ACTLF(JISSSYU1)  ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績週集計Ｆ　ＡＣＴ１】'
              GOTO ABEND END
    ACTLF FILE-JISSSYU2.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## ACTLF(JISSSYU2)  ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績週集計Ｆ　ＡＣＴ２】'
              GOTO ABEND END
    ACTLF FILE-JISSSYU3.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## ACTLF(JISSSYU3)  ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績週集計Ｆ　ＡＣＴ３】'
              GOTO ABEND END
    ACTLF FILE-JISSSYU4.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## ACTLF(JISSSYU4)  ERR ##',TO-XCTL
              ?KEKA4  :=  '【実績週集計Ｆ　ＡＣＴ４】'
              GOTO ABEND END

RTN:

    ?KBN   :=  ?OK
    ?KEKA1 :=  '処理が正常終了しました。'
    ?KEKA2 :=  '更新結果等を確認して下さい。'
    ?KEKA3 :=  ''
    ?KEKA4 :=  '（正常終了）'
    CALL SMG0020L.TOKELIB
                    ,PARA-(?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL
    RETURN    PGMEC-@PGMEC

ABEND:

    ?KBN   :=  ?NG
    ?KEKA1 :=  '受払累積データの月次更新が異常終了しま'
    ?KEKA2 :=  'した。この画面のリストを出力して下さい'
    ?KEKA3 :=  '（ログリストを採取しＮＡＶへ連絡）'
    CALL SMG0020L.TOKELIB
                    ,PARA-(?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=   '### ' && ?PGMID && ' ABEND' &&   '    ###'
    ?MSG(2)   :=   '###' && ' PGMEC = ' &&
                    %SBSTR(?PGMECX,8,4) &&         '      ###'
    ?MSG(3)   :=   '###' && ' STEP = '  && ?STEP
                                                   && '   ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL
    END

    RETURN    PGMEC-@PGMEC
