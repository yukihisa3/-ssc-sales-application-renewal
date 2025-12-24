****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　出荷　　　　　　　　　　　　　　  *
*    サブシステム　　　　：　コーナン　　　ＥＤＩ　　　　　　  *
*    モジュール名　　　　：　箱数ファイル（手書）作成　　　　　*
*    　　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
*    作成日／作成者　　　：　2021/09/14 INOUE                  *
*    処理概要　　　　　　：　抽出データより箱数ファイル　　　　*
*                            データを作成する（全件対象）　　　*
*    更新履歴                                                  *
*    更新日／更新者　　　：　                                  *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY1321B.
*                  流用:SSY1221B
 AUTHOR.                NAV.
 DATE-WRITTEN.          2021/09/14.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 手書伝票抽出ワーク >>--*
     SELECT   JHTTEGKN  ASSIGN    TO        DA-01-S-JHTTEGKN
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE  STATUS   IS   TEG-STATUS.
*----<< 店舗マスタ >>----*
     SELECT   TENMS1    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TEN-F52   TEN-F011
                        FILE  STATUS   IS   TEN-STATUS.
*----<< 箱数ファイル（手書） >>----*
     SELECT   KTHAKOF   ASSIGN    TO        DA-01-VI-KTHAKOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       HAK-F001  HAK-F002
                                            HAK-F003  HAK-F004
                                            HAK-FA05
                        FILE      STATUS    HAK-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    手書伝票抽出ワーク
******************************************************************
 FD  JHTTEGKN           LABEL     RECORD   IS   STANDARD.
     COPY     JHTTEGKN  OF        XFDLIB
              JOINING   TEG       PREFIX.
******************************************************************
*    店舗マスタ
******************************************************************
 FD  TENMS1             LABEL     RECORD   IS   STANDARD.
     COPY     TENMS1    OF        XFDLIB
              JOINING   TEN  AS   PREFIX.
******************************************************************
*    箱数ファイル（手書）
******************************************************************
 FD  KTHAKOF            LABEL     RECORD   IS   STANDARD.
     COPY     KTHAKOF   OF        XFDLIB
              JOINING   HAK  AS   PREFIX.
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  SKIP-CNT            PIC  9(08)     VALUE  ZERO.
     03  SKIP2-CNT           PIC  9(08)     VALUE  ZERO.
     03  WRT-CNT             PIC  9(08)     VALUE  ZERO.
     03  RWT-CNT             PIC  9(08)     VALUE  ZERO.
     03  KMK2-CNT            PIC  9(08)     VALUE  ZERO.
     03  KMS-CNT             PIC  9(08)     VALUE  ZERO.
 01  WK-INV-FLG.
     03  KTHAKOF-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  SHTDENLA-INV-FLG    PIC  X(03)     VALUE  SPACE.
     03  TENMS1-INV-FLG      PIC  X(03)     VALUE  SPACE.
 01  WK-GYO-CNT              PIC  9(02)     VALUE  ZERO.
 01  WK-KMS-F06              PIC  9(09)     VALUE  ZERO.
*
*退避
 01  BK-C128-REC             PIC  X(128)    VALUE  SPACE.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME           PIC   9(08)  VALUE  ZERO.
*日付の編集
 01  WK-HDATE.
     03  WK-HDATE1         PIC 9(02).
     03  WK-HDATE2         PIC 9(06).
 01  WK-NDATE.
     03  WK-NDATE1         PIC 9(02).
     03  WK-NDATE2         PIC 9(06).
*
*ブレイク項目
 01  BRK-F07               PIC  9(05)   VALUE ZERO.
 01  BRK-F01               PIC  9(08)   VALUE ZERO.
 01  BRK-F131              PIC  X(02)   VALUE SPACE.
 01  BRK-F111              PIC  9(08)   PACKED-DECIMAL VALUE ZERO.
 01  BRK-F112              PIC  9(08)   PACKED-DECIMAL VALUE ZERO.
*
 01  WK-ST.
     03  TEG-STATUS        PIC  X(02).
     03  DEN-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
     03  IN-STATUS         PIC  X(02).
     03  HAK-STATUS        PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY1321B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY1321B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY1321B".
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
 01  PARA-IN-BUMON             PIC   X(04).
 01  PARA-IN-TANTOU            PIC   X(02).
 01  PARA-IN-JDATE             PIC   9(08).
 01  PARA-IN-JTIME             PIC   9(04).
 01  PARA-IN-TORICD            PIC   9(08).
 01  PARA-IN-SOKO              PIC   X(02).
*01  PARA-IN-NDATE             PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING
                                  PARA-IN-BUMON
                                  PARA-IN-TANTOU
                                  PARA-IN-JDATE
                                  PARA-IN-JTIME
                                  PARA-IN-TORICD
                                  PARA-IN-SOKO.
*                                 PARA-IN-NDATE.
*
******************************************************************
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHTTEGKN.
     MOVE      "JHTTEGKN "   TO   AB-FILE.
     MOVE      TEG-STATUS    TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TENMS1.
     MOVE      "TENMS1"     TO   AB-FILE.
     MOVE      TEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KTHAKOF.
     MOVE      "KTHAKOF"   TO   AB-FILE.
     MOVE      HAK-STATUS   TO   AB-STS.
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
     OPEN     INPUT     JHTTEGKN  TENMS1.
     OPEN     I-O       KTHAKOF.
*
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FLG   WK-CNT.
     MOVE     SPACE     TO        WK-INV-FLG SHTDENLA-INV-FLG
                                             TENMS1-INV-FLG.
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
*
*   システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*
*    手書伝票抽出ワーク読込み
     PERFORM JHTTEGKN-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　手書伝票抽出ワーク読込み　　　　　　　　　　　　*
****************************************************************
 JHTTEGKN-READ-SEC    SECTION.
*
     MOVE    "JHTTEGKN-READ-SEC"    TO  S-NAME.
*
     READ     JHTTEGKN
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  JHTTEGKN-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*
 JHTTEGKN-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"              TO   S-NAME.
*
*　　プラス数量のみ対象
     IF      TEG-F15   <=   0
             GO                      TO  MAIN999
     END-IF.
*
     IF    ( TEG-F07    =   BRK-F07   ) AND
           ( TEG-F01    =   BRK-F01   ) AND
           ( TEG-F131   =   BRK-F131  ) AND
           ( TEG-F111   =   BRK-F111  ) AND
           ( TEG-F112   =   BRK-F112  )
*----------- PERFORM    JHTTEGKN-REWRITE-SEC
             GO         TO      MAIN999
     ELSE
             GO         TO      MAIN888
     END-IF.
*
 MAIN888.
*
     PERFORM KTHAKOF-WRITE-SEC.
     MOVE    TEG-F07                 TO  BRK-F07.
     MOVE    TEG-F01                 TO  BRK-F01.
     MOVE    TEG-F131                TO  BRK-F131.
     MOVE    TEG-F111                TO  BRK-F111.
     MOVE    TEG-F112                TO  BRK-F112.
*--- PERFORM JHTTEGKN-REWRITE-SEC.
*
 MAIN999.
*    手書伝票抽出ワーク読込み
     PERFORM JHTTEGKN-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　箱数ファイル出力処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 KTHAKOF-WRITE-SEC  SECTION.
*
     MOVE     "KTHAKOF-WRITE-SEC"  TO      S-NAME.
*
     MOVE      SPACE               TO      HAK-REC.
     INITIALIZE                            HAK-REC.
     MOVE      PARA-IN-JDATE       TO      HAK-F001.
     MOVE      PARA-IN-JTIME       TO      HAK-F002.
     MOVE      PARA-IN-TORICD      TO      HAK-F003.
     MOVE      PARA-IN-SOKO        TO      HAK-F004.
     MOVE      TEG-F07             TO      HAK-FA01.
     MOVE      TEG-F01             TO      HAK-FA02.
     IF        TEG-F01  =  23631
               MOVE        2363    TO      HAK-FA02
     END-IF.
     MOVE      TEG-F131            TO      HAK-FA03.
     MOVE      TEG-F111            TO      HAK-FA04.
     MOVE      TEG-F112            TO      HAK-FA05.
     MOVE      TEG-F12(1:2)        TO      HAK-FA06.
     MOVE      TEG-F30             TO      HAK-FA07.
     MOVE      "ｻｶﾀﾉﾀﾈ"            TO      HAK-FA08.
     MOVE      PARA-IN-TORICD      TO      TEN-F52.
     MOVE      TEG-F07             TO      TEN-F011.
     READ      TENMS1
       INVALID
               MOVE   ALL NC"＊"   TO      HAK-FA09
       NOT INVALID
               MOVE   TEN-F02      TO      HAK-FA09
     END-READ.
     MOVE      NC"株式会社サカタのタネ"
                                   TO      HAK-FA10.
     MOVE      TEG-F1421           TO      HAK-FA11.
     MOVE      SPACE               TO      HAK-FA12.
     MOVE      SYS-DATEW           TO      HAK-FC01.
     MOVE      WK-TIME(1:6)        TO      HAK-FC02.
     MOVE      PARA-IN-BUMON       TO      HAK-FC03.
     MOVE      PARA-IN-TANTOU      TO      HAK-FC04.
     WRITE     HAK-REC.
     ADD       1                   TO      WRT-CNT.
*
 KTHAKOF-WRITE-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY  NC"手書伝票データ" "IN  = " READ-CNT UPON CONS.
     DISPLAY  NC"箱数ファイル　" "OUT = " WRT-CNT  UPON CONS.
*    DISPLAY  NC"基本情報Ｆ" "RWT = " RWT-CNT   UPON CONS.
*
     CLOSE     JHTTEGKN  KTHAKOF   TENMS1.
*
     DISPLAY   MSG-END   UPON CONS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
