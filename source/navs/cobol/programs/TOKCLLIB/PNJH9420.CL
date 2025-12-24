/.************************************************************  ./
/. *   サカタのタネ　　　　　　　　　　　　　　　            *  ./
/. *   SYSTEM-NAME :    ヨドバシ　　　ＥＤＩ                 *  ./
/. *   JOB-ID      :    PNJH9420                             *  ./
/. *   JOB-NAME    :    ヨドバシ受注データ変換               *  ./
/. *   UPDATE      :    2021/07/26 S22461250                 *  ./
/. *   UPDATE      :    2023/01/20 S22480910                 *  ./
/. *                    NJH9450B→NJH9450B（変更）           *  ./
/. *                              SSY9430V（追加）           *  ./
/. *                              FIMPORT （追加）           *  ./
/. ***********************************************************  ./
    PGM (P1-?HIDUKE,P2-?JIKAN,P3-?PGCHK)

/.##ﾊﾟﾗﾒﾀ定義##./
    PARA      ?HIDUKE   ,STRING*8,IN,VALUE-'        ' /.受信日付./
    PARA      ?JIKAN    ,STRING*4,IN,VALUE-'    '  /.受信時間./
    PARA      ?PGCHK    ,STRING*1,OUT,VALUE-' '    /.ｴﾗｰﾁｪｯｸ./
/.##ﾜｰｸﾃｲｷﾞ##./
    VAR       ?TOKCD    ,STRING*8,VALUE-'        ' /.受信取引先./
    VAR       ?LINE     ,STRING*1,VALUE-' '        /.回線./
    VAR       ?YUSEN    ,STRING*1,VALUE-' '        /.回線優先./
    VAR       ?LIBNM    ,STRING*8,VALUE-'        ' /.集信LIB./
    VAR       ?FILNM    ,STRING*8,VALUE-'        ' /.集信FILE./
    VAR       ?JKEKA    ,STRING*4,VALUE-'    '     /.結果./
    VAR       ?PGMEC    ,INTEGER                   /.ﾘﾀｰﾝｺｰﾄﾞ./
    VAR       ?PGMECX   ,STRING*11                 /.ﾘﾀｰﾝｺｰﾄﾞ変換./
    VAR       ?PGMEM    ,STRING*99                 /.ﾘﾀｰﾝ名称./
    VAR       ?MSG      ,STRING*99(6)              /.ﾒｯｾｰｼﾞ退避ﾜｰｸ./
    VAR       ?MSGX     ,STRING*99                 /.ﾒｯｾｰｼﾞ表示用./
    VAR       ?PGMID    ,STRING*8,VALUE-'PNJH9420' /.PROGRAM-ID./
    VAR       ?STEP     ,STRING*8                  /.STEP-ID./
    VAR       ?HTIME    ,STRING*8,VALUE-'        ' /.ｼｽﾃﾑ時間退避用./
    VAR       ?HTIMES   ,STRING*4,VALUE-'    '     /.変換開始時間./
    VAR       ?HTIMEE   ,STRING*4,VALUE-'    '     /.変換終了時間./
    VAR       ?TIMES    ,STRING*4,VALUE-'0000'     /.受信開始./
    VAR       ?TIMEE    ,STRING*4,VALUE-'0000'     /.受信終了./
    VAR       ?SDATE    ,STRING*8,VALUE-'00000000' /.ｼｽﾃﾑ日付./
    VAR       ?STIME    ,STRING*6,VALUE-'000000'   /.ｼｽﾃﾑ時刻./
    VAR       ?JIKDATEF ,STRING*8,VALUE-'00000000' /.実行日付./
    VAR       ?JIKDATET ,STRING*8,VALUE-'00000000' /.実行日付./
    VAR       ?JIKTIMEF ,STRING*6,VALUE-'000000'   /.実行時刻./
    VAR       ?JIKTIMET ,STRING*6,VALUE-'000000'   /.実行時刻./
    VAR       ?JIKKBN   ,STRING*1,VALUE-'1'        /.実行区分./
    VAR       ?SYORINM1 ,STRING*40
    VAR       ?SYORINM2 ,STRING*50
    VAR       ?SYORINM3 ,STRING*50
    VAR       ?SYORIKN1 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN2 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN3 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN4 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN5 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKNC ,STRING*7,VALUE-'0000000'
    VAR       ?ERR      ,STRING*2,VALUE-'  '
    VAR       ?JOB      ,STRING*1,VALUE-'2'      /.2:受注ﾃﾞｰﾀ取込./
/.##ﾃﾞｰﾀ変換PG用ﾊﾟﾗﾒﾀ##./
    VAR       ?PARA     ,STRING*14,VALUE-'              '
/.##結果FLG用##./
    VAR       ?KEKA     ,STRING*4,VALUE-'    '     /.結果FLGﾊﾟﾗﾒﾀ./
/.##ﾌｧｲﾙ変換ﾜｰｸ##./
    VAR       ?LIBN     ,NAME                      /.ﾗｲﾌﾞﾗﾘ名前型./
    VAR       ?FILN     ,NAME                      /.ﾌｧｲﾙ名前型./
    VAR       ?FILLIB   ,NAME!MOD                  /.ﾌｧｲﾙ拡張用./
    VAR       ?FILID    ,STRING*17                 /.ﾌｧｲﾙ名表示用./

    VAR       ?SDTYMD   ,STRING*8,VALUE-'00000000' /. 開始作成日 ./
    VAR       ?EDTYMD   ,STRING*8,VALUE-'99999999' /. 終了作成日 ./
    VAR       ?SYTANCD  ,STRING*2,VALUE-'  '      /. 処理担当者 ./
    VAR       ?KBN      ,STRING*1,VALUE-'1'       /. ｵﾝﾗｲﾝ／手書 ./
    VAR       ?KANRINO  ,STRING*8,VALUE-'        ' /. 管理番号 ./
    VAR       ?BACHINO  ,STRING*20,VALUE-'                    '
                                                  /.バッチＮｏ./
    VAR       ?BUMON    ,STRING*4,VALUE-'    '    /.部門名./
    VAR       ?TANCD    ,STRING*2,VALUE-'  '      /.担当者CD./
    VAR       ?KEKKA    ,STRING*2,VALUE-'  '      /.結果./
    /.エラーメール発行用ワーク./
    VAR ?ERKBN    ,STRING*03,VALUE-'   '       /.区分./
    VAR ?ERDATE   ,STRING*08,VALUE-'00000000'  /.バッチ日付./
    VAR ?ERTIME   ,STRING*04,VALUE-'0000'      /.バッチ時刻./
    VAR ?ERTKCD   ,STRING*08,VALUE-'00000000'  /.バッチ取引先./

/.##ﾌﾟﾛｸﾞﾗﾑｶｲｼﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    SNDMSG MSG-'＊＊＊＊＊＊＊＊＊＊＊＊',TO-XCTL
    SNDMSG MSG-'＊　データ変換開始　　＊',TO-XCTL
    SNDMSG MSG-'＊＊＊＊＊＊＊＊＊＊＊＊',TO-XCTL

    DEFLIBL TOKELIB/TOKDLIB/TOKFLIB/TOKELIBO/TOKJLIB/TOKKLIB/TOKDTLIB

/.## ログインユーザー情報取得 ##./
SIT9000B:

    ?STEP :=   'SIT9000B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANCD)
    ?PGMEC    :=   @PGMEC
    IF        ?PGMEC    ^=   0    THEN
              /.##ABENDｺｰﾄﾞｾｯﾄ##./
              ?ERR  := 'ST'
              ?KEKA := 'K525'
              CALL PGM-SNJ0730B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
              GOTO ABEND
    END

/.  データ変換                                                  ./
NCV9401B:

    ?STEP :=   'NCV9401B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-JSMDAYL1,TOFILE-JSMDAYL1.TOKJLIB
    CALL      PGM-NCV9401B.TOKSOLIB,PARA-(?HIDUKE,?JIKAN,?TOKCD,?LINE,
                                   ?YUSEN,?LIBNM,?FILNM,?JKEKA)
    ?PGMEC    :=   @PGMEC
    IF        ?PGMEC    ^=   0    THEN
              ?ERR  := 'ST'
              GOTO ABEND
    END

/.##ﾃﾞｰﾀ変換PGﾍのﾊﾟﾗﾒﾀ作成##./
    ?PARA :=   ?HIDUKE && ?JIKAN && ?LINE && ?YUSEN
    SNDMSG ?PARA,TO-XCTL.@ORGPROF,JLOG-@YES

/.##受信ﾌｧｲﾙの編集##./
    ?FILN   :=  %NAME(?FILNM)
    ?LIBN   :=  %NAME(?LIBNM)
    ?FILLIB :=  %NCAT(?FILN,?LIBN)
    ?FILID  :=  %STRING(?FILLIB)
    SNDMSG ?FILID,TO-XCTL.@ORGPROF,JLOG-@YES

   /.##変換開始時間取得##./
    ?HTIME  := @STIME
    ?HTIMES := %SBSTR(?HTIME,1,4)

    CALL SCVMSG.TOKELIB,PARA-('ｼﾞｭｼﾝDT ﾍﾝｶﾝ   START')

/.  SORTF                                                       ./
SORTF1:

    ?STEP :=   'SORTF1  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    SORTF  INFILE-?FILLIB,
           OUTFILE-YODORDSF.TOKDTLIB,
           KEY-'F01!A',
           KEY1-'F04!A',
           KEY2-'F03!A',
           KEY3-'F02!A',
           KEY4-'F07!A',
           SELECT-'F01!EQ!#ORDERS',
           RCDL-@DSP
    ?PGMEC    :=   @PGMEC
    IF        ?PGMEC    ^=   0    THEN
              ?ERR  := 'ST'
              GOTO ABEND
    END
    ?MSGX :=  'SORTF-OUT  '
    SNDMSG    ?MSGX,TO-XCTL

SORTF2:

    ?STEP :=   'SORTF2  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    SORTF  INFILE-?FILLIB,
           OUTFILE-YODCHGSF.TOKDTLIB,
           KEY-'F01!A',
           KEY1-'F04!A',
           KEY2-'F03!A',
           KEY3-'F02!A',
           KEY4-'F07!A',
           SELECT-'F01!EQ!#ORDCHG',
           RCDL-@DSP
    ?PGMEC    :=   @PGMEC
    IF        ?PGMEC    ^=   0    THEN
              ?ERR  := 'ST'
              GOTO ABEND
    END
    ?MSGX :=  'SORTF-OUT  '
    SNDMSG    ?MSGX,TO-XCTL

/.  ヨドバシ受注データ変換（発注）                              ./
NJH9400B:

    ?STEP :=   'NJH9400B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-NJH9400B.TOKSOLIB,PARA-(?HIDUKE,?JIKAN,
                                          ?BUMON,?TANCD,
                                          ?TOKCD,?SYORIKN1,
                                          ?SYORIKN2,?ERR)
    ?PGMEC    :=   @PGMEC
    SNDMSG    ?HIDUKE,TO-XCTL.@ORGPROF,JLOG-@YES
    SNDMSG    ?JIKAN,TO-XCTL.@ORGPROF,JLOG-@YES
    SNDMSG    ?BUMON,TO-XCTL.@ORGPROF,JLOG-@YES
    SNDMSG    ?TANCD,TO-XCTL.@ORGPROF,JLOG-@YES
    SNDMSG    ?TOKCD,TO-XCTL.@ORGPROF,JLOG-@YES
    SNDMSG    ?SYORIKN1,TO-XCTL.@ORGPROF,JLOG-@YES
    SNDMSG    ?SYORIKN2,TO-XCTL.@ORGPROF,JLOG-@YES
    SNDMSG    ?ERR,TO-XCTL.@ORGPROF,JLOG-@YES
    IF        ?PGMEC    ^=   0    THEN
              /.##ABENDｺｰﾄﾞｾｯﾄ##./
              ?ERR  := 'ST'
              ?KEKA := 'K525'
              CALL PGM-SNJ0730B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
              GOTO DATESET
    ELSE
         IF   ?ERR       =  'OK'  THEN
              /.##正常終了ｺｰﾄﾞｾｯﾄ##./
              ?KEKA := 'K522'
              CALL PGM-SNJ0730B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
              GOTO NJH9450B
         END
         IF   ?ERR       =  'WG'  THEN
              /.##正常終了ｺｰﾄﾞｾｯﾄ##./
              ?KEKA := 'K522'
              CALL PGM-SNJ0730B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
              GOTO NJH9450B
         ELSE
              /.##ABENDｺｰﾄﾞｾｯﾄ##./
              ?KEKA := 'K525'
              CALL PGM-SNJ0730B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
              GOTO DATESET
         END
    END

NJH9450B:

    ?STEP :=   'NJH9450B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-YODORDL1,TOFILE-YODORDL1.TOKDTLIB
    OVRF      FILE-YODORDL2,TOFILE-YODORDL2.TOKDTLIB
    OVRF      FILE-JHSHENL1,TOFILE-JHSHENL1.TOKFLIB
    OVRF      FILE-YODJOHL1,TOFILE-YODJOHL1.TOKDTLIB
    OVRF      FILE-JSMDAYL1,TOFILE-JSMDAYL1.TOKJLIB
    OVRF      FILE-JSMKENL1,TOFILE-JSMKENL1.TOKJLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF      FILE-JHMNITL1,TOFILE-JHMNITL1.TOKFLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRF      FILE-YODNKKL1,TOFILE-YODNKKL1.TOKDTLIB
    OVRVLDF   FILE-VLD500,TOFILE-LD500.XUCL

    CALL      PGM-NJH9450B.TOKSOLIB,PARA-(?PARA,?TANCD,?SDATE,?STIME)
    ?PGMEC    :=   @PGMEC
    IF        ?PGMEC    ^=   0    THEN
              /.##ABENDｺｰﾄﾞｾｯﾄ##./
              ?ERR  := 'ST'
              ?KEKA := 'K525'
              CALL PGM-SNJ0730B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
              GOTO ABEND
    ELSE
              /.##正常終了ｺｰﾄﾞｾｯﾄ##./
              ?KEKA := 'K522'
              CALL PGM-SNJ0730B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
    END

/.##ヨドバシ納期変更出荷確定確認CSV出力##./
SSY9430V:

    ?STEP :=   'SSY9430V'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?JIKDATEF := ?SDATE
    ?JIKDATET := ?SDATE
    ?JIKTIMEF := ?STIME
    ?JIKTIMET := ?STIME
    CALL SSY9430V.TOKSOLIB,PARA-(?JIKDATEF,?JIKDATET,
                                 ?JIKTIMEF,?JIKTIMET,
                                 ?JIKKBN,?SYORIKNC)

    ?PGMEC := @PGMEC
    IF        ?PGMEC  ^=  0  THEN
              ?ERR  := 'ST'
              ?KEKA   :=    'K519'
              GOTO    ABEND
    END

    ?MSGX :=  'ＣＳＶ件数　＝' && ?SYORIKNC
    SNDMSG    ?MSGX,TO-XCTL

    IF        ?SYORIKNC = '0000000' THEN
              GOTO      SSN0050L
    END

/.##ヨドバシ納期変更出荷確定確認CSV転送##./
FIMPORT1:

    ?STEP :=   'FIMPORT1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-YODNKKCV.TOKDTLIB,
            TYPE-@FILE,
            PARA-YODOBASI,
            UNIT-4,
            OPR-@NO

    ?PGMEC := @PGMEC
    IF        ?PGMEC  ^=  0  THEN
              ?ERR  := 'ST'
              ?KEKA   :=    'K519'
              GOTO    ABEND
    END

SSN0050L:
  /.取込結果リスト ./
    DEFLIBL TOKELIBO/TOKELIB
    OVRPRTF FILE-PRTF,TOFILE-PRTF.TOKELIB,MEDLIB-TOKELIBO
    ?SYORINM1 := 'ヨドバシ受注データ変換・伝票データ作成　'
    ?SYORINM2 := 'ヨドバシ受注データ取込　終了しました。'
    ?SYORINM3 := '件数１：受注件数　件数２：受注変更件数'
    CALL SSN0050L.TOKELIBO,
          PARA-(?SYORINM1,?SYORINM2,?SYORINM3,?SYORIKN1,?SYORIKN2,
                ?SYORIKN3,?SYORIKN4,?SYORIKN5)

    DEFLIBL TOKELIB/TOKDLIB/TOKFLIB/TOKELIBO/TOKJLIB/TOKKLIB/TOKDTLIB

    CALL SCVMSG.TOKELIB,PARA-('ｼﾞｭｼﾝDT ﾍﾝｶﾝ   END  ')
   /.##変換開始時間取得##./
    ?HTIME  := @STIME
    ?HTIMEE := %SBSTR(?HTIME,1,4)

/.##受信時間開始／終了、変換時間開始／終了更新##./
    CALL SCVMSG.TOKELIB,PARA-('ﾍﾝｶﾝ TIME SET       ')

DATESET:

    ?STEP :=   'DATESET '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF    FILE-JSMDAYL1,TOFILE-JSMDAYL1.TOKJLIB
    CALL    PGM-NCV0170B.TOKELIBO,PARA-(?HIDUKE,?JIKAN,?TOKCD,
                                       ?TIMES,?TIMEE,?HTIMES,?HTIMEE)
    ?PGMEC := @PGMEC
    IF   ?PGMEC ^= 0 THEN
         ?ERR  := 'ST'
         ?KEKA := 'K519'
         GOTO      ABEND
    END

    /. 0件の場合はCSV出力しない./
    IF        ?SYORIKN1 = '0000000' THEN
        IF        ?SYORIKN2 = '0000000' THEN
                   GOTO    ABEND
        END
    END

/.##ヨドバシ発注データチェック結果出力##  ./
    CALL SCVMSG.TOKELIB,PARA-('ﾍﾝｶﾝ CSV OUTPUT     ')
NJH9410B:

    ?STEP :=   'NJH9410B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL      NJH9410B.TOKSOLIB
    ?PGMEC := @PGMEC
    IF        ?PGMEC  ^=  0  THEN
              ?ERR  := 'ST'
              ?KEKA   :=    'K519'
              GOTO    ABEND
    END

CNVFILE1:

    ?STEP :=   'CNVFILE1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CNVFILE   FILE-YODCHKSF.TOKDTLIB,
              TOFILE-YODCHKCS.TOKDTLIB,
              ADD-@YES
    ?PGMEC := @PGMEC
    IF        ?PGMEC  ^=  0  THEN
              ?ERR  := 'ST'
              ?KEKA   :=    'K519'
              GOTO    ABEND
    END

/.##ＣＳＶ転送##./
    CALL SCVMSG.TOKELIB,PARA-('ﾍﾝｶﾝ CSV ﾃﾝｿｳ       ')
FIMPORT2:

    ?STEP :=   'FIMPORT2'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-YODCHKCS.TOKDTLIB,
            TYPE-@FILE,
            PARA-YODOBASI,
            UNIT-2,
            OPR-@NO
    ?PGMEC := @PGMEC
    IF        ?PGMEC  ^=  0  THEN
              ?ERR  := 'ST'
              ?KEKA   :=    'K519'
              GOTO    ABEND
    END

/.##件数リスト出力##./
    CALL SCVMSG.TOKELIB,PARA-('ｹﾝｽｳ LST       START')
KENLSTAZ:

    ?STEP :=   'KENLSTAZ'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SBMJOB JOB-KENLSTAZ,JOBD-CVCS.XUCL,JOBK-@B,PGM-KENLST.TOKCLIBO,
           VSIZE-5,RSIZE-16,LOG-@YES!1024,PARA-(?HIDUKE,?JIKAN,
           ?TOKCD,?LINE,?YUSEN)


/.##ﾌﾟﾛｸﾞﾗﾑ正常終了##./
RTN:

    IF     ?ERR   =   'ER'   THEN
           GOTO   ABEND
    END

    CALL SCVMSG.TOKELIB,PARA-('ｾｲｼﾞｮｳ ｼｭｳﾘｮｳ SJH310')

   /.##ｴﾗｰﾘｽﾄ発行##./
    SBMJOB JOB-KEKALIST,JOBD-CVCS.XUCL,JOBK-@B,
           PGM-SNJ0590L.TOKELIBO,LOG-@YES!1024,PTY-5,PGMEL-@I/@L/
           @S/@T,LIBL-TOKFLIB/TOKELIB/TOKJLIB/TOKELIBO,
           PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?PGCHK := '0'
    IF     ?ERR   =   'ER'   THEN
           ?PGCHK :=  '2'
    END
    IF     ?ERR   =   'OK'   THEN
           ?PGCHK :=  '1'
    END
    IF     ?ERR   =   'WG'   THEN
           ?PGCHK :=  '1'
    END
    IF     ?ERR   =   'NO'   THEN
           ?PGCHK :=  '0'
    END

    RETURN    PGMEC-?PGMEC

/.##異常終了時##./
ABEND:

  /.## エラーメール用データ出力　セット 開始  ##./
    ?ERKBN   :=  '035'
    ?ERDATE  :=  ?HIDUKE
    ?ERTIME  :=  ?JIKAN
    ?ERTKCD  :=  '00000000'
    CALL ERR0010B.TOKSOLIB,PARA-(?ERKBN,?ERDATE,?ERTIME,?ERTKCD)
  /.## エラーメール用データ出力　セット 終了  ##./

    CALL SCVMSG.TOKELIB,PARA-('ｲｼﾞｮｳ ｼｭｳﾘｮｳ  SJH310')

   /.##ｴﾗｰ結果更新##./
    DEFLIBL TOKELIB/TOKDLIB/TOKFLIB/TOKELIBO/TOKJLIB/TOKKLIB/TOKDTLIB
    CALL PGM-NCV0090B.TOKELIBO,PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
   /.##ｴﾗｰﾘｽﾄ発行##./
    SBMJOB JOB-KEKALIST,JOBD-CVCS.XUCL,JOBK-@B,
           PGM-SNJ0590L.TOKELIBO,LOG-@YES!1024,PTY-5,PGMEL-@I/@L/
           @S/@T,LIBL-TOKFLIB/TOKELIB/TOKJLIB/TOKELIBO,
           PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=    '### ' && ?PGMID && ' ABEND' && ' ###'
    ?MSG(2)   :=    '### ' && ' PGMEC = ' &&
                     %SBSTR(?PGMECX,8,4) && ' ###'
    ?MSG(3)   :=    '###' && ' LINE = '  && %LAST(LINE)      && ' ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    END

    IF     ?ERR   =   'ER'   THEN
           ?PGCHK :=  '2'
    END
    IF     ?ERR   =   'OK'   THEN
           ?PGCHK :=  '1'
    END
    IF     ?ERR   =   'NO'   THEN
           ?PGCHK :=  '0'
    END
    IF     ?ERR   =   'ST'  THEN
           ?PGCHK :=  '3'
    END

    ?KEKA := ?JKEKA

    RETURN    PGMEC-@PGMEC
