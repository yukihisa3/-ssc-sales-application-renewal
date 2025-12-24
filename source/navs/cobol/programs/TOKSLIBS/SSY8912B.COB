****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    サブシステム　　　　：　リック　　　　　　ＷＥＢＥＤＩ　　
*    業務名　　　　　　　：　リック　　　　　　ＷＥＢＥＤＩ　　
*    モジュール名　　　　：　納品予定データ作成
*    作成日／更新日　　　：　2008/04/14
*    作成者／更新者　　　：　NAV SATO
*    処理概要　　　　　　：　受け取ったパラメタより対象データ
*                            を出荷情報保存データより抽出する。
*    更新履歴            ：
*      2011/10/07 飯田/NAV 基幹サーバ統合
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY8912B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          08/04/14.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*売上伝票データ
     SELECT   SHTDENF   ASSIGN    TO        DA-01-VI-SHTDENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       DEN-F01   DEN-F02
                                            DEN-F04   DEN-F051
                                       *> 2011/10/07,S  S.I/NAV
                                            DEN-F07   DEN-F112
                                       *> 2011/10/07,E  S.I/NAV
                                            DEN-F03
                        FILE      STATUS    DEN-STATUS.
*リック　発注情報保存データ
     SELECT   RCJOHOF   ASSIGN    TO        DA-01-VI-RCJOHOL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       RCJ-K01   RCJ-K02
                                            RCJ-K03   RCJ-K04
                                            RCJ-K08   RCJ-K05
                                            RCJ-K06   RCJ-K07
                        FILE      STATUS    RCJ-STATUS.
*リック　出荷情報修正
     SELECT   RCSYUKF   ASSIGN    TO        DA-01-VI-RCSYUKL5
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       RCS-K01   RCS-K02
                                            RCS-K03   RCS-K04
                                            RCS-K08   RCS-K05
                                            RCS-K06   RCS-K07
                        FILE      STATUS    RCS-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    売上伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  SHTDENF
                        LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
*
******************************************************************
*    リック　　　　　発注情報保存データ
******************************************************************
 FD  RCJOHOF            LABEL RECORD   IS   STANDARD.
     COPY     RCJOHOF   OF        XFDLIB
              JOINING   RCJ       PREFIX.
******************************************************************
*    リック　　　　　発注データ
******************************************************************
 FD  RCSYUKF            LABEL RECORD   IS   STANDARD.
     COPY     RCSYUKF   OF        XFDLIB
              JOINING   RCS       PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  SKIP1-CNT           PIC  9(08)     VALUE  ZERO.
     03  SKIP2-CNT           PIC  9(08)     VALUE  ZERO.
     03  KOS1-CNT            PIC  9(08)     VALUE  ZERO.
     03  KMK2-CNT            PIC  9(08)     VALUE  ZERO.
     03  KMS-CNT             PIC  9(08)     VALUE  ZERO.
 01  WK-INV-FLG.
     03  RCSYUKF-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  SHTDENF-INV-FLG     PIC  X(03)     VALUE  SPACE.
 01  WK-GYO-CNT              PIC  9(02)     VALUE  ZERO.
 01  WK-KMS-F06              PIC  9(09)     VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  RCJ-STATUS        PIC  X(02).
     03  RCS-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY8912B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY8912B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY8912B".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*納品予定数量
 01  WK-NOUHIN.
     03  FILLER            PIC  X(08)   VALUE  SPACE.
     03  NOU-F01           PIC  9(07)   VALUE  ZERO.
     03  FILLER            PIC  X(08)   VALUE  SPACE.
*****03  NOU-F02           PIC  9(09)   VALUE  ZERO.
     03  NOU-F02           PIC  9(07)   VALUE  ZERO.
     03  FILLER            PIC  X(11)   VALUE  SPACE.
*納品予定原価金額
 01  WK-GENKA.
     03  GEN-F01           PIC  9(09)   VALUE  ZERO.
*****03  FILLER            PIC  X(02)   VALUE  SPACE.
*納品予定原価金額
 01  WK-BAIKA              PIC  9(09)   VALUE  ZERO.
*納品予定日項目セット
 01  WK-A05.
     03  WK-A051           PIC  X(04)   VALUE  SPACE.
     03  WK-A052           PIC  9(08)   VALUE  ZERO.
     03  WK-A053           PIC  9(08)   VALUE  ZERO.
     03  WK-A054           PIC  X(24)   VALUE  ZERO.
     03  WK-A055           PIC  X(09)   VALUE  ZERO.
     03  WK-A056           PIC  X(230)  VALUE  ZERO.
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-JDATE             PIC   9(08).
 01  PARA-JTIME             PIC   9(04).
 01  PARA-TORICD            PIC   9(08).
 01  PARA-SOKO              PIC   X(02).
 01  PARA-NOUDT             PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE
                                       PARA-JTIME
                                       PARA-TORICD
                                       PARA-SOKO
                                       PARA-NOUDT.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHTDENF.
     MOVE      "SHTDENL1"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   RCJOHOF.
     MOVE      "RCJOHOL2 "   TO   AB-FILE.
     MOVE      RCJ-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   RCSYUKF.
     MOVE      "RCSYUKL5 "   TO   AB-FILE.
     MOVE      RCS-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FLG   =  "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     I-O       RCJOHOF  SHTDENF.
     OPEN     I-O       RCSYUKF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FLG   WK-CNT.
     MOVE     SPACE     TO        WK-INV-FLG.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*    リック　　　　　発注情報保存データスタート
     MOVE     SPACE          TO   RCJ-REC.
     INITIALIZE                   RCJ-REC.
     MOVE     PARA-JDATE     TO   RCJ-K01.
     MOVE     PARA-JTIME     TO   RCJ-K02.
     MOVE     PARA-TORICD    TO   RCJ-K03.
     MOVE     PARA-SOKO      TO   RCJ-K04.
     MOVE     PARA-NOUDT     TO   RCJ-K08.
     MOVE     ZERO           TO   RCJ-K05.
     MOVE     ZERO           TO   RCJ-K06.
     MOVE     ZERO           TO   RCJ-K07.
     START    RCJOHOF   KEY  >=   RCJ-K01   RCJ-K02
                                  RCJ-K03   RCJ-K04
                                  RCJ-K08   RCJ-K05
                                  RCJ-K06   RCJ-K07
         INVALID   KEY
              MOVE    "END"  TO   END-FLG
              GO   TO   INIT-EXIT
     END-START.
*    リック　　　　　発注情報保存データ読込み
     PERFORM RCJOHOF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 RCJOHOF-READ-SEC    SECTION.
*
     READ     RCJOHOF
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  RCJOHOF-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*    バッチ番号のチェック
     IF       PARA-JDATE  =  RCJ-K01
     AND      PARA-JTIME  =  RCJ-K02
     AND      PARA-TORICD =  RCJ-K03
              CONTINUE
     ELSE
              MOVE     "END"        TO  END-FLG
              GO                    TO  RCJOHOF-READ-EXIT
     END-IF.
*    取引先のチェック
     IF       PARA-TORICD  =  ZERO
              CONTINUE
     ELSE
              IF   PARA-TORICD  =  RCJ-K03
                   CONTINUE
              ELSE
                   MOVE     "END"        TO  END-FLG
                   GO                    TO  RCJOHOF-READ-EXIT
              END-IF
     END-IF.
*    送信ＦＬＧのチェック
     IF       RCJ-K11  =  "1"
              GO                 TO   RCJOHOF-READ-SEC
     END-IF.
*    倉庫ＣＤチェック
     IF       PARA-SOKO   =  SPACE
              CONTINUE
     ELSE
              IF   PARA-SOKO  =  RCJ-K04
                   CONTINUE
              ELSE
                   GO            TO  RCJOHOF-READ-SEC
              END-IF
     END-IF.
*    納品日のチェック
     IF       PARA-NOUDT  =  ZERO
              CONTINUE
     ELSE
              IF  PARA-NOUDT  =  RCJ-K08
                  CONTINUE
              ELSE
                  GO        TO   RCJOHOF-READ-SEC
              END-IF
     END-IF.
*
 RCJOHOF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*    売上伝票ファイル検索
     MOVE     RCJ-K03             TO   DEN-F01.
     MOVE     RCJ-K06             TO   DEN-F02.
     MOVE     0                   TO   DEN-F04.
     MOVE     "40"                TO   DEN-F051.
     MOVE     RCJ-K07             TO   DEN-F03.
* 2011/10/07,S  S.I/NAV
     MOVE  RCJ-K05          TO  DEN-F07.  *> 店舗CD
     MOVE  RCJ-K08          TO  DEN-F112. *> 納入日
* 2011/10/07,E  S.I/NAV
     READ     SHTDENF    INVALID
              MOVE    "INV"       TO   SHTDENF-INV-FLG
              NOT  INVALID
              MOVE    SPACE       TO   SHTDENF-INV-FLG
     END-READ.
*
     IF       SHTDENF-INV-FLG  =  "INV"
              ADD     1           TO   SKIP2-CNT
              GO                  TO   MAIN010
     ELSE
**************数量確認(発注数<>出荷数の時ﾌｧｲﾙに出力)
              PERFORM   RCSYUKF-READ-SEC
              IF    RCSYUKF-INV-FLG = "INV"
                    PERFORM   RCSYUKF-WRITE-SEC
                    MOVE   "1"       TO   RCJ-K09
                    MOVE  SYS-DATEW  TO   RCJ-K10
                    REWRITE  RCJ-REC
              END-IF
     END-IF.
 MAIN010.
*    リック 発注情報保存データ読込み
     PERFORM RCJOHOF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　リック　発注確定データ作成処理
****************************************************************
 RCSYUKF-WRITE-SEC     SECTION.
*
     MOVE     "RCSYUKF-WRITE-SEC"  TO  S-NAME.
*
     MOVE      SPACE               TO  RCS-REC.
     INITIALIZE                        RCS-REC.
     MOVE      RCJ-REC             TO  RCS-REC.
*    原単価
*****COMPUTE   RCS-M16  =   RCJ-M16 / 100 .
     COMPUTE   RCS-M16  =   RCJ-M16.
*    納品数量
     MOVE     SPACE                TO  WK-NOUHIN.
     INITIALIZE                        WK-NOUHIN.
     MOVE      DEN-F15             TO  NOU-F01.
     MOVE      NOU-F01             TO  RCS-M12  RCJ-M12.
*    納品原価金額
     MOVE     SPACE                TO  WK-GENKA.
     INITIALIZE                        WK-GENKA.
     COMPUTE   GEN-F01  =  DEN-F15  *  ( RCS-M16 / 100 ).
*****COMPUTE   GEN-F01  =  DEN-F15  *    RCS-M16.
     MOVE      WK-GENKA            TO  RCS-M14.
*    納品売価金額
     MOVE      ZERO                TO  WK-BAIKA.
     COMPUTE   WK-BAIKA =  DEN-F15  *  RCS-M17.
     MOVE      WK-BAIKA            TO  RCS-M15 RCJ-M15.
*    欠品区分
     IF        RCS-M10  NOT =  RCS-M12
               MOVE   "1"          TO  RCS-M09
     END-IF.
*
     WRITE     RCS-REC.
     ADD       1                   TO  KOS1-CNT.
*
 RCSYUKF-WRITE-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY "ﾘｯｸ  F      READ CNT = " READ-CNT  UPON CONS.
     DISPLAY "ｳﾘｱｹﾞDT ﾅｼ           CNT = " SKIP2-CNT UPON CONS.
     DISPLAY "ﾘｯｸ  ｶｸﾃｲDT WT   CNT = " KOS1-CNT  UPON CONS.
*
     CLOSE     SHTDENF  RCJOHOF  RCSYUKF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 RCSYUKF-READ-SEC    SECTION.
*
     MOVE     SPACE          TO   RCS-REC.
     INITIALIZE                   RCS-REC.
     MOVE     RCJ-K01        TO   RCS-K01.
     MOVE     RCJ-K02        TO   RCS-K02.
     MOVE     RCJ-K03        TO   RCS-K03.
     MOVE     RCJ-K04        TO   RCS-K04.
     MOVE     RCJ-K08        TO   RCS-K08.
     MOVE     RCJ-K05        TO   RCS-K05.
     MOVE     RCJ-K06        TO   RCS-K06.
     MOVE     RCJ-K07        TO   RCS-K07.
     MOVE     RCJ-K08        TO   RCS-K08.
     READ     RCSYUKF    INVALID
              MOVE    "INV"       TO   RCSYUKF-INV-FLG
              NOT  INVALID
              MOVE    SPACE       TO   RCSYUKF-INV-FLG
     END-READ.
*
 RCSYUKF-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
