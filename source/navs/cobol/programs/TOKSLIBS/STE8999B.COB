****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　リック　ＷＥＢＥＤＩ　　　　　　　*
*    業務名　　　　　　　：　リック　ＷＥＢＥＤＩ              *
*    モジュール名　　　　：　手書納品予定データＦＬＧ解除　　　*
*    作成日／更新日　　　：　2008/04/21                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　受け取ったパラメタより対象データ  *
*                            を削除する。　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            STE8999B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          08/04/21.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*リック　　　　　出荷確定データ
     SELECT   RCSYUKF   ASSIGN    TO        DA-01-VI-RCSYUKL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       RCS-K01   RCS-K02
                                            RCS-K03   RCS-K04
                                            RCS-K08   RCS-K06
                                            RCS-K07
                        FILE      STATUS    RCS-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    リック　　　　　出荷確定データ
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
     03  DEL-CNT             PIC  9(08)     VALUE  ZERO.
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
     03  RCS-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "STE8999B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "STE8999B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "STE8999B".
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
 01  PARA-DENNOS            PIC   9(09).
 01  PARA-DENNOE            PIC   9(09).
 01  PARA-SOKO              PIC   X(02).
 01  PARA-NOUDT             PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE
                                       PARA-JTIME
                                       PARA-TORICD
                                       PARA-DENNOS
                                       PARA-DENNOE
                                       PARA-SOKO
                                       PARA-NOUDT.
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   RCSYUKF.
     MOVE      "RCSYUKF "   TO   AB-FILE.
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
*    リック　　　　　出荷確定ファイルスタート
     MOVE     SPACE          TO   RCS-REC.
     INITIALIZE                   RCS-REC.
     MOVE     PARA-JDATE     TO   RCS-K01.
     MOVE     PARA-JTIME     TO   RCS-K02.
     MOVE     PARA-TORICD    TO   RCS-K03.
     MOVE     PARA-SOKO      TO   RCS-K04.
*****MOVE     ZERO           TO   RCS-K05.
*********DISPLAY "PARA-DENNOS=" PARA-DENNOS UPON CONS.
     MOVE     PARA-DENNOS    TO   RCS-K06.
     MOVE     ZERO           TO   RCS-K07.
     MOVE     PARA-NOUDT     TO   RCS-K08.
     START    RCSYUKF   KEY  >=   RCS-K01   RCS-K02
                                  RCS-K03   RCS-K04
                                  RCS-K08   RCS-K06
                                  RCS-K07
         INVALID   KEY
              DISPLAY NC"＃対象データがありません＃" UPON CONS
              MOVE    "END"  TO   END-FLG
              GO   TO   INIT-EXIT
     END-START.
*    リック　　　　　出荷確定データ読込み
     PERFORM RCSYUKF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 RCSYUKF-READ-SEC    SECTION.
*
     READ     RCSYUKF
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  RCSYUKF-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*    バッチ番号のチェック
     IF       99999999    =  RCS-K01
     AND      9999        =  RCS-K02
     AND      PARA-TORICD =  RCS-K03
              CONTINUE
     ELSE
              MOVE     "END"        TO  END-FLG
              GO                    TO  RCSYUKF-READ-EXIT
     END-IF.
*    取引先のチェック
     IF       PARA-TORICD  =  ZERO
              CONTINUE
     ELSE
              IF   PARA-TORICD  =  RCS-K03
                   CONTINUE
              ELSE
                   MOVE     "END"        TO  END-FLG
                   GO                    TO  RCSYUKF-READ-SEC
              END-IF
     END-IF.
*    伝票番号のチェック
     IF       PARA-DENNOS  =  ZERO
              CONTINUE
     ELSE
*                  DISPLAY "PARA-DENNOS=" PARA-DENNOS UPON CONS
*                  DISPLAY "RCS-K06    =" RCS-K06     UPON CONS
*                  DISPLAY "PARA-DENNOE=" PARA-DENNOE UPON CONS
              IF   PARA-DENNOS  <=  RCS-K06    AND
                   RCS-K06      <=  PARA-DENNOE
                   CONTINUE
              ELSE
*                  DISPLAY "AAA" UPON CONS
                   GO                    TO  RCSYUKF-READ-SEC
              END-IF
     END-IF.
*    送信ＦＬＧのチェック
*****IF       RCS-K11  =  "1"
*****         GO                 TO   RCSYUKF-READ-SEC
*****END-IF.
*    倉庫ＣＤチェック
     IF       PARA-SOKO   =  SPACE
              CONTINUE
     ELSE
              IF   PARA-SOKO  =  RCS-K04
                   CONTINUE
              ELSE
                   GO            TO  RCSYUKF-READ-SEC
              END-IF
     END-IF.
*    納品日のチェック
     IF       PARA-NOUDT  =  ZERO
              CONTINUE
     ELSE
              IF  PARA-NOUDT  =  RCS-K08
                  CONTINUE
              ELSE
                  GO        TO   RCSYUKF-READ-SEC
              END-IF
     END-IF.
*
 RCSYUKF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*    リックＦＬＧ解除
     MOVE    "0"                  TO   RCS-K11.
     MOVE    ZERO                 TO   RCS-K12.
     REWRITE RCS-REC.
     ADD      1                   TO   DEL-CNT.
 MAIN010.
*    リック　　　　　出荷確定データ読込み
     PERFORM RCSYUKF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY "ﾘｯｸ ｶｸﾃｲDT READ CNT = " READ-CNT  UPON CONS.
     DISPLAY "ﾘｯｸ ｶｸﾃｲDT OFF  CNT = " DEL-CNT   UPON CONS.
*
     CLOSE     RCSYUKF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
