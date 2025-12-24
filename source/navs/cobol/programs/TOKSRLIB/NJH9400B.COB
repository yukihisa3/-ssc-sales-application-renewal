****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    業務名　　　　　　　：　ＥＤＩ　　　　　　　　　　　　　　
*    サブシステム　　　　：　ヨドバシ　ＥＤＩ　　　　　　　
*    モジュール名　　　　：　ヨドバシ発注データ取込チェック
*    作成日／作成者　　　：　2021/07/20 INOUE
*    処理概要　　　　　　：　当回分の受注データを発注・発注変更
*                            に振り分け・チェック。後方処理可能
*                            な状態にする。
*    更新日／更新者　　　：　2021/07/21 INOUE
*    更新内容　　　　　　：　ＩＮＰＵＴ分割、参照追加　　　　　
*    更新日／更新者　　　：　2023/04/20 TAKAHASHI
*    更新内容　　　　　　：　商品名項目追加
*                            　　　　　　　　　　　　　　　　　
****************************************************************
*
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           NJH9400B.
 AUTHOR.               NAV-ASSIST.
 DATE-WRITTEN.         2021/07/20.
*
 ENVIRONMENT           DIVISION.
 CONFIGURATION         SECTION.
 SOURCE-COMPUTER.      FUJITSU.
 OBJECT-COMPUTER.      FUJITSU.
 SPECIAL-NAMES.
     CONSOLE IS        CONS.
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*ヨドバシ受信データ（発注）
     SELECT   YODORDSF  ASSIGN    TO    DA-01-S-YODORDSF
                        ACCESS MODE    IS    SEQUENTIAL
                        ORGANIZATION   IS    SEQUENTIAL
                        FILE   STATUS  IS    JYO-ST.
*ヨドバシ受信データ（発注変更）
     SELECT   YODCHGSF  ASSIGN    TO    DA-01-S-YODCHGSF
                        ACCESS MODE    IS    SEQUENTIAL
                        ORGANIZATION   IS    SEQUENTIAL
                        FILE   STATUS  IS    JYC-ST.
*ヨドバシ取込ワーク（発注）
     SELECT   YODORDPF  ASSIGN    TO   DA-01-VS-YODORDPF
                        ACCESS MODE    IS    SEQUENTIAL
                        ORGANIZATION   IS    SEQUENTIAL
                        FILE   STATUS  IS    ORD-ST.
*ヨドバシ取込ワーク（発注）参照用
     SELECT   YODORDL1  ASSIGN    TO   DA-01-VI-YODORDL1
                        ACCESS MODE    IS    RANDOM
                        ORGANIZATION   IS    INDEXED
                        RECORD    KEY        OR1-F03
                                             OR1-F02
                                             OR1-F07
                        WITH   DUPLICATES
                        FILE   STATUS  IS    OR1-ST.
*ヨドバシ取込ワーク（発注変更）
     SELECT   YODCHGPF  ASSIGN    TO   DA-01-VS-YODCHGPF
                        ACCESS MODE    IS    SEQUENTIAL
                        ORGANIZATION   IS    SEQUENTIAL
                        FILE   STATUS  IS    CHG-ST.
*ヨドバシ取込チェックデータ
     SELECT   YODCHKSF  ASSIGN    TO   DA-01-S-YODCHKSF
                        ACCESS MODE    IS    SEQUENTIAL
                        ORGANIZATION   IS    SEQUENTIAL
                        FILE   STATUS  IS    CHK-ST.
*ヨドバシ基本情報ファイル
     SELECT   YODJOHL1  ASSIGN    TO        DA-01-VI-YODJOHL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JOH-F17
                                            JOH-F03
                                            JOH-F02
                                            JOH-F07
                        FILE      STATUS    JOH-ST.
*店舗マスタ
     SELECT   TENMS1    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TEN-F52
                                            TEN-F011
                        FILE      STATUS    TEN-ST.
*売上伝票ファイル
     SELECT   SHTDENLS  ASSIGN    TO        DA-01-VI-SHTDENLS
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       DEN-F01
                                            DEN-F111
                                            DEN-F02
                                            DEN-F03
                        FILE STATUS    IS   DEN-ST.
*SUB商品変換ＴＢＬ
     SELECT   SUBTBLL1  ASSIGN    TO        DA-01-VI-SUBTBLL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SUB-F01
                                            SUB-F02
                        FILE STATUS    IS   SUB-ST.
*
******************************************************************
*                                                                *
*    DATA              DIVISION                                  *
*                                                                *
******************************************************************
*
 DATA                  DIVISION.
 FILE                  SECTION.
*ヨドバシ受信データ（発注）
 FD  YODORDSF          BLOCK CONTAINS 11   RECORDS
                       LABEL RECORD   IS   STANDARD.
     COPY              YODORDSF       OF   XFDLIB
     JOINING           JYO            AS   PREFIX.
*
*ヨドバシ受信データ（発注変更）
 FD  YODCHGSF          BLOCK CONTAINS 11   RECORDS
                       LABEL RECORD   IS   STANDARD.
     COPY              YODCHGSF       OF   XFDLIB
     JOINING           JYC            AS   PREFIX.
*
*ヨドバシ取込ワーク（発注）
 FD  YODORDPF
                       LABEL RECORD   IS   STANDARD.
     COPY              YODORDPF       OF   XFDLIB
     JOINING           ORD            AS   PREFIX.
*
*ヨドバシ取込ワーク（発注）参照用
 FD  YODORDL1
                       LABEL RECORD   IS   STANDARD.
     COPY              YODORDL1       OF   XFDLIB
     JOINING           OR1            AS   PREFIX.
*
*ヨドバシ取込ワーク（発注変更）
 FD  YODCHGPF
                       LABEL RECORD   IS   STANDARD.
     COPY              YODCHGPF       OF   XFDLIB
     JOINING           CHG            AS   PREFIX.
*
*ヨドバシ取込チェックデータ
 FD  YODCHKSF          BLOCK CONTAINS 11   RECORDS
                       LABEL RECORD   IS   STANDARD.
     COPY              YODCHKSF       OF   XFDLIB
     JOINING           CHK            AS   PREFIX.
*
*ヨドバシ基本情報ファイル
 FD  YODJOHL1
                       LABEL RECORD   IS   STANDARD.
     COPY              YODJOHL1       OF   XFDLIB
     JOINING           JOH            AS   PREFIX.
*
*店舗マスタ
 FD  TENMS1
                       LABEL RECORD   IS   STANDARD.
     COPY              TENMS1         OF   XFDLIB
     JOINING           TEN            AS   PREFIX.
*
*売上伝票ファイル
 FD  SHTDENLS
                       LABEL RECORD   IS   STANDARD.
     COPY              SHTDENLS       OF   XFDLIB
     JOINING           DEN            AS   PREFIX.
*
*SUB商品変換ＴＢＬ
 FD  SUBTBLL1
                       LABEL RECORD   IS   STANDARD.
     COPY              SUBTBLF        OF   XFDLIB
     JOINING           SUB            AS   PREFIX.
*
******************************************************************
*                 WORKING-STORAGE   SECTION
******************************************************************
 WORKING-STORAGE       SECTION.
*
 77  JYO-ST                PIC  X(02)     VALUE  "00".
 77  JYC-ST                PIC  X(02)     VALUE  "00".
 77  ORD-ST                PIC  X(02)     VALUE  "00".
 77  OR1-ST                PIC  X(02)     VALUE  "00".
 77  CHG-ST                PIC  X(02)     VALUE  "00".
 77  CHK-ST                PIC  X(02)     VALUE  "00".
 77  JOH-ST                PIC  X(02)     VALUE  "00".
 77  TEN-ST                PIC  X(02)     VALUE  "00".
 77  DEN-ST                PIC  X(02)     VALUE  "00".
 77  SUB-ST                PIC  X(02)     VALUE  "00".
 77  END-FLG-ORD           PIC  X(03)     VALUE  "   ".
 77  END-FLG-CHG           PIC  X(03)     VALUE  "   ".
 01  RD-CNT-ORD            PIC  9(07)     VALUE  ZERO.
 01  RD-CNT-CHG            PIC  9(07)     VALUE  ZERO.
 01  WT-CNT            PIC  9(07)     VALUE  ZERO.
 01  WT-CNT-CHG            PIC  9(07)     VALUE  ZERO.
 01  OK-FLG                PIC  X(02)     VALUE  "  ".
 01  JOH-HIT-FLG           PIC  X(03)     VALUE  "   ".
 01  JYO-HIT-FLG           PIC  X(03)     VALUE  "   ".
 01  TEN-HIT-FLG           PIC  X(03)     VALUE  "   ".
 01  DEN-HIT-FLG           PIC  X(03)     VALUE  "   ".
 01  SUB-HIT-FLG           PIC  X(03)     VALUE  "   ".
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE           PIC   9(06)  VALUE  ZERO.
     03  SYS-DATEW          PIC   9(08)  VALUE  ZERO.
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME            PIC   9(08)  VALUE  ZERO.
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
     03  LINK-OUT2-RET      PIC   X(01).
     03  LINK-OUT2-YMD8     PIC   9(08).
*
*店舗コード
 01  WK-TEN-F011.
     03  WK-TEN-F011-1      PIC   9(01)  VALUE  ZERO.
     03  WK-TEN-F011-2      PIC   9(04)  VALUE  ZERO.
 01  WK-TEN-F011-R REDEFINES WK-TEN-F011.
     03  WK-TEN-F011-RR     PIC   9(05).
*
*01  WK-HEAD.
*  03  WK-HEAD-01          PIC X(722) VALUE SPACE.
*
*    COPY                  NFHACSF2       OF   XFDLIB
*    JOINING               WK             AS   PREFIX.
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NJH9400B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NJH9400B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NJH9400B".
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
     03  MSG-IN-ORD.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " ORDERS= ".
         05  IN-CNT-ORD     PIC   9(07).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-IN-CHG.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " ORDCHG= ".
         05  IN-CNT-CHG     PIC   9(07).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE "    OUT= ".
         05  OUT-CNT        PIC   9(07).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
 LINKAGE SECTION.
   01  LINK-IN-BDATE              PIC 9(08).
   01  LINK-IN-BTIME              PIC 9(04).
   01  LINK-IN-BUMCD              PIC X(04).
   01  LINK-IN-TANCD              PIC X(02).
   01  LINK-IN-TORCD              PIC 9(08).
   01  LINK-OUT-KENSU1            PIC 9(07).
   01  LINK-OUT-KENSU2            PIC 9(07).
   01  LINK-OUT-KEKKA             PIC X(02).
******************************************************************
*                                                                *
*    PROCEDURE         DIVISION                                  *
*                                                                *
******************************************************************
*
 PROCEDURE             DIVISION   USING
                                         LINK-IN-BDATE
                                         LINK-IN-BTIME
                                         LINK-IN-BUMCD
                                         LINK-IN-TANCD
                                         LINK-IN-TORCD
                                         LINK-OUT-KENSU1
                                         LINK-OUT-KENSU2
                                         LINK-OUT-KEKKA.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   YODORDSF.
     MOVE      "YODORDSF"   TO   AB-FILE.
     MOVE      JYO-ST       TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     MOVE     "ST"          TO   LINK-OUT-KEKKA.
     STOP      RUN.
*
 FILEERR-SEC11          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   YODCHGSF.
     MOVE      "YODCHGSF"   TO   AB-FILE.
     MOVE      JYC-ST       TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     MOVE     "ST"          TO   LINK-OUT-KEKKA.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   YODORDPF.
     MOVE      "YODORDPF"   TO   AB-FILE.
     MOVE      ORD-ST       TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     MOVE     "ST"          TO   LINK-OUT-KEKKA.
     STOP      RUN.
*
 FILEERR-SEC21          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   YODORDL1.
     MOVE      "YODORDL1"   TO   AB-FILE.
     MOVE      OR1-ST       TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     MOVE     "ST"          TO   LINK-OUT-KEKKA.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   YODCHGPF.
     MOVE      "YODCHGPF"   TO   AB-FILE.
     MOVE      CHG-ST       TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     MOVE     "ST"          TO   LINK-OUT-KEKKA.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   YODCHKSF.
     MOVE      "YODCHKSF"   TO   AB-FILE.
     MOVE      CHK-ST       TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     MOVE     "ST"          TO   LINK-OUT-KEKKA.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   YODJOHL1.
     MOVE      "YODJOHL1"   TO   AB-FILE.
     MOVE      JOH-ST       TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     MOVE     "ST"          TO   LINK-OUT-KEKKA.
     STOP      RUN.
*
 FILEERR-SEC6           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TENMS1.
     MOVE      "TENMS1 "   TO   AB-FILE.
     MOVE      TEN-ST       TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     MOVE     "ST"          TO   LINK-OUT-KEKKA.
     STOP      RUN.
*
 FILEERR-SEC7           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHTDENLS.
     MOVE      "SHTDENLS "   TO   AB-FILE.
     MOVE      DEN-ST       TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     MOVE     "ST"          TO   LINK-OUT-KEKKA.
     STOP      RUN.
*
 FILEERR-SEC8           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SUBTBLL1.
     MOVE      "SUBTBLL1"   TO   AB-FILE.
     MOVE      SUB-ST       TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     MOVE     "ST"          TO   LINK-OUT-KEKKA.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
 PRG-CONTROL  SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM      INIT-SEC.
     PERFORM      MAIN-ORD-SEC.
     PERFORM      MAIN-CHG-SEC.
     PERFORM      END-SEC.
     STOP RUN.
*
*----------------------------------------------------------------*
*       LEVEL     1    ｲﾆｼｬﾙ ｼｮﾘ                                 *
*----------------------------------------------------------------*
*
 INIT-SEC  SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     DISPLAY  MSG-START UPON CONS.
*
 INIT-0.
     OPEN    INPUT   YODORDSF YODCHGSF
                     YODJOHL1 TENMS1 SHTDENLS SUBTBLL1.
     OPEN    OUTPUT  YODORDPF YODCHGPF YODCHKSF.
*    OPEN    INPUT   YODORDL1.
*
 INIT-1.
*   システム日付編集*
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
*   システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*
 INIT-21.
     READ    YODORDSF
       AT END
             DISPLAY   NC"＃＃＃＃＃＃＃＃＃＃＃" UPON CONS
             DISPLAY   NC"＃　発注データ０件　＃" UPON CONS
             DISPLAY   NC"＃＃＃＃＃＃＃＃＃＃＃" UPON CONS
             MOVE     "END"       TO      END-FLG-ORD
*            MOVE     "ER"        TO      LINK-OUT-KEKKA
*            STOP      RUN
     END-READ.
*
 INIT-22.
     READ    YODCHGSF
       AT END
             DISPLAY   NC"＃＃＃＃＃＃＃＃＃＃＃＃＃" UPON CONS
             DISPLAY   NC"＃　発注変更データ０件　＃" UPON CONS
             DISPLAY   NC"＃＃＃＃＃＃＃＃＃＃＃＃＃" UPON CONS
             MOVE     "END"       TO      END-FLG-CHG
*            MOVE     "ER"        TO      LINK-OUT-KEKKA
*            STOP      RUN
     END-READ.
*
 INIT-3.
     IF      END-FLG-ORD = "   "
             MOVE    "OK"       TO      LINK-OUT-KEKKA
             ADD     1          TO      RD-CNT-ORD
     END-IF.
     IF      END-FLG-CHG = "   "
             MOVE    "OK"       TO      LINK-OUT-KEKKA
             ADD     1          TO      RD-CNT-CHG
     END-IF.
     IF    ( END-FLG-ORD  = "END" ) AND
           ( END-FLG-CHG  = "END" )
             MOVE    "NO"       TO      LINK-OUT-KEKKA
             STOP     RUN
     END-IF.
*
 INIT-EXT.
     EXIT.
*
*----------------------------------------------------------------*
*       LEVEL     1    ﾒｲﾝ   ｼｮﾘ  ORDERS                         *
*----------------------------------------------------------------*
*
 MAIN-ORD-SEC   SECTION.
     MOVE  "MAIN-ORD-SEC"         TO   S-NAME.
*
     IF     END-FLG-ORD = "END"
            GO                    TO   MAIN-ORD-EXT
     END-IF.
*
 MAIN-ORD-01.
*  発注伝票日付　常識チェック
     MOVE        "2"              TO   LINK-IN-KBN.
     MOVE         JYO-F03         TO   LINK-IN-YMD8.
     CALL        "SKYDTCKB"    USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD8.
*T
*    DISPLAY "LINK-OUT-RET=" LINK-OUT-RET UPON CONS.
*T
*
 MAIN-ORD-02.
*  納入期日　常識チェック
     IF         ( JYO-F14   NOT   NUMERIC ) OR
                ( JYO-F14   =     0       )
                  GO              TO   MAIN-ORD-03
     END-IF.
     MOVE        "2"              TO   LINK-IN-KBN.
     MOVE         JYO-F14         TO   LINK-IN-YMD8.
     CALL        "SKYDTCKB"    USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT2-RET
                                       LINK-OUT2-YMD8.
*T
*    DISPLAY "LINK-OUT2-RET=" LINK-OUT-RET UPON CONS.
*T
*
 MAIN-ORD-03.
*  基本情報ファイル存在チェック
     MOVE         LINK-IN-TORCD   TO   JOH-F17.
     MOVE         JYO-F03         TO   JOH-F03.
     MOVE         JYO-F02         TO   JOH-F02.
     MOVE         JYO-F07         TO   JOH-F07.
     PERFORM      JOH-READ-SEC.
*TEST
*    DISPLAY "ORD JOH-F17=" LINK-IN-TORCD UPON CONS.
*    DISPLAY "    JOH-F03=" JYO-F03 UPON CONS.
*    DISPLAY "    JOH-F02=" JYO-F02 UPON CONS.
*    DISPLAY "    JOH-F07=" JYO-F07 UPON CONS.
*    DISPLAY "    HIT-FLG=" JOH-HIT-FLG UPON CONS.
*TEST
*
 MAIN-ORD-04.
*  店舗マスタ（物流センター）存在チェック（発注）
     IF           JYO-F01     NOT =    "ORDERS"
                  GO              TO   MAIN-ORD-05
     END-IF.
     MOVE         LINK-IN-TORCD   TO   TEN-F52.
     MOVE         JYO-F06(1:4)    TO   WK-TEN-F011-2
     MOVE         WK-TEN-F011-RR  TO   TEN-F011.
     PERFORM      TEN-READ-SEC.
*
 MAIN-ORD-05.
*  SUB商品変換ＴＢＬ存在チェック
*    IF           JYO-F01     NOT =    "ORDERS"
*                 GO              TO   MAIN-ORD-06
*    END-IF.
     MOVE         LINK-IN-TORCD   TO   SUB-F01.
     MOVE         JYO-F08         TO   SUB-F02.
     PERFORM      SUB-READ-SEC.
*
 MAIN-ORD-06.
*  売上伝票ファイル計上済みチェック（発注変更）
     IF           JYO-F01     NOT =    "ORDCHG"
                  GO              TO   MAIN-ORD-07
     END-IF.
     IF           JOH-HIT-FLG NOT =    "HIT"
                  MOVE  "   "     TO   DEN-HIT-FLG
                  GO              TO   MAIN-ORD-07
     END-IF.
     MOVE         LINK-IN-TORCD   TO   DEN-F01.
     MOVE         JYO-F03         TO   DEN-F111.
     MOVE         JOH-F18         TO   DEN-F02.
     MOVE         JOH-F19         TO   DEN-F03.
     PERFORM      DEN-READ-SEC.
*
*
 MAIN-ORD-07.
*  レコード編集・出力
     MOVE         SPACE           TO   ORD-REC CHG-REC CHK-REC.
     INITIALIZE                        ORD-REC CHG-REC CHK-REC.
     MOVE        ","              TO   CHK-K01 CHK-K02 CHK-K03
                                       CHK-K04 CHK-K05 CHK-K06
                                       CHK-K07 CHK-K08 CHK-K09
                                       CHK-K10 CHK-K11 CHK-K12
                                       CHK-K13 CHK-K14 CHK-K15
                                       CHK-K16 CHK-K17 CHK-K18
                                       CHK-K19 CHK-K20 CHK-K21
                                       CHK-K22 CHK-K23 CHK-K24
                                       CHK-K25 CHK-K26 CHK-K27
                                       CHK-K28 CHK-K29 CHK-K30
                                       CHK-K31 CHK-K32 CHK-K33
                                       CHK-K34 CHK-K35.
*  メッセージタイプ
     MOVE         JYO-F01         TO   CHK-F01.
     EVALUATE     JYO-F01
         WHEN "ORDERS"
                  MOVE   JYO-F01  TO   ORD-F01
         WHEN "ORDCHG"
                  MOVE   JYO-F01  TO   CHG-F01
         WHEN  OTHER
                  MOVE   "E"      TO   CHK-F22
     END-EVALUATE.
*
*  購買発注番号
     MOVE         JYO-F02         TO   CHK-F02.
     EVALUATE     JYO-F01
         WHEN "ORDERS"
                  MOVE   JYO-F02  TO   ORD-F02
         WHEN "ORDCHG"
                  MOVE   JYO-F02  TO   CHG-F02
     END-EVALUATE.
*
*  発注伝票日付
     MOVE         JYO-F03         TO   CHK-F03.
     IF           LINK-OUT-RET    =    ZERO
                  MOVE    " "     TO   CHK-F23
     ELSE
                  MOVE    "E"     TO   CHK-F23
                  MOVE    "ER"    TO   LINK-OUT-KEKKA
     END-IF.
     EVALUATE     JYO-F01
         WHEN "ORDERS"
                  MOVE   JYO-F03  TO   ORD-F03
         WHEN "ORDCHG"
                  MOVE   JYO-F03  TO   CHG-F03
     END-EVALUATE.
*
*  仕入先コード・発注先コード
     MOVE         JYO-F04         TO   CHK-F04.
     MOVE         JYO-F05         TO   CHK-F05.
     EVALUATE     JYO-F01
         WHEN "ORDERS"
                  MOVE   JYO-F04  TO   ORD-F04
                  MOVE   JYO-F05  TO   ORD-F05
         WHEN "ORDCHG"
                  MOVE   JYO-F04  TO   CHG-F04
                  MOVE   JYO-F05  TO   CHG-F05
     END-EVALUATE.
*
*  出荷先コード
     MOVE         JYO-F06         TO   CHK-F06.
     IF           TEN-HIT-FLG     =    "HIT"
                  MOVE    " "     TO   CHK-F28
     ELSE
                  MOVE    "E"     TO   CHK-F28
                  MOVE    "ER"    TO   LINK-OUT-KEKKA
     END-IF.
     EVALUATE     JYO-F01
         WHEN "ORDERS"
                  MOVE   JYO-F06  TO   ORD-F06
         WHEN "ORDCHG"
                  MOVE   JYO-F06  TO   CHG-F06
     END-EVALUATE.
*
*  明細行番号
     MOVE         JYO-F07         TO   CHK-F07.
     IF         ( JYO-F07  IS NUMERIC ) AND
                ( JYO-F07  NOT = 0    )
                  MOVE    " "     TO   CHK-F24
     ELSE
                  MOVE    "E"     TO   CHK-F24
                  MOVE    "ER"    TO   LINK-OUT-KEKKA
     END-IF.
     EVALUATE     JYO-F01
         WHEN "ORDERS"
                  MOVE   JYO-F07  TO   ORD-F07
         WHEN "ORDCHG"
                  MOVE   JYO-F07  TO   CHG-F07
     END-EVALUATE.
*
*  品目番号
     MOVE         JYO-F08         TO   CHK-F08.
     IF           SUB-HIT-FLG     =    "HIT"
                  MOVE    " "     TO   CHK-F29
     ELSE
                  MOVE    "E"     TO   CHK-F29
***               MOVE    "ER"    TO   LINK-OUT-KEKKA
                  CONTINUE
     END-IF.
     EVALUATE     JYO-F01
         WHEN "ORDERS"
                  MOVE   JYO-F08  TO   ORD-F08
         WHEN "ORDCHG"
                  MOVE   JYO-F08  TO   CHG-F08
     END-EVALUATE.
*
*  明細フリーテキスト１（キャンセル区分）
     MOVE         JYO-F09         TO   CHK-F09.
     EVALUATE     JYO-F01
         WHEN "ORDERS"
                  MOVE   JYO-F09  TO   ORD-F09
         WHEN "ORDCHG"
                  MOVE   JYO-F09  TO   CHG-F09
     END-EVALUATE.
*
*  明細単価
     MOVE         JYO-F10         TO   CHK-F10.
     IF           JYO-F01         =   "ORDCHG"
        IF        JOH-HIT-FLG     =   "HIT"
           IF     JYO-F10         =    JOH-F10
                  MOVE    " "     TO   CHK-F32
           ELSE
                  MOVE    "W"     TO   CHK-F32
                  IF       LINK-OUT-KEKKA  = "OK"
                           MOVE    "WG"    TO   LINK-OUT-KEKKA
                  END-IF
           END-IF
        END-IF
     END-IF.
     EVALUATE     JYO-F01
         WHEN "ORDERS"
                  MOVE   JYO-F10  TO   ORD-F10
         WHEN "ORDCHG"
                  MOVE   JYO-F10  TO   CHG-F10
     END-EVALUATE.
*
*  お客様伝票番号
     MOVE         JYO-F11         TO   CHK-F11.
     EVALUATE     JYO-F01
         WHEN "ORDERS"
                  MOVE   JYO-F11  TO   ORD-F11
         WHEN "ORDCHG"
                  MOVE   JYO-F11  TO   CHG-F11
     END-EVALUATE.
*
*  配送場所
     MOVE         JYO-F12         TO   CHK-F12.
     EVALUATE     JYO-F01
         WHEN "ORDERS"
                  MOVE   JYO-F12  TO   ORD-F12
         WHEN "ORDCHG"
                  MOVE   JYO-F12  TO   CHG-F12
     END-EVALUATE.
*
*  発注数量
     MOVE         JYO-F13         TO   CHK-F13.
     IF           JYO-F13  IS NUMERIC
         IF       JYO-F01  =  "ORDERS"
             IF   JYO-F13  NOT = 0
                  MOVE    " "     TO   CHK-F25
             ELSE
                  MOVE    "E"     TO   CHK-F25
                  MOVE    "ER"    TO   LINK-OUT-KEKKA
             END-IF
         END-IF
     ELSE
                  MOVE    "E"     TO   CHK-F25
                  MOVE    "ER"    TO   LINK-OUT-KEKKA
     END-IF.
     EVALUATE     JYO-F01
         WHEN "ORDERS"
                  MOVE   JYO-F13  TO   ORD-F13
         WHEN "ORDCHG"
                  MOVE   JYO-F13  TO   CHG-F13
     END-EVALUATE.
*
*  納入期日
     MOVE         JYO-F14         TO   CHK-F14.
     IF         ( JYO-F14  IS NUMERIC ) AND
                ( JYO-F14  NOT = 0    )
         IF       LINK-OUT2-RET    =    ZERO
                  MOVE    " "     TO   CHK-F26
         ELSE
                  MOVE    "E"     TO   CHK-F26
                  MOVE    "ER"    TO   LINK-OUT-KEKKA
         END-IF
     END-IF.
     EVALUATE     JYO-F01
         WHEN "ORDERS"
                  MOVE   JYO-F14  TO   ORD-F14
         WHEN "ORDCHG"
                  MOVE   JYO-F14  TO   CHG-F14
     END-EVALUATE.
*
*  バッチ（日）（時）（取）
*  倉庫
     EVALUATE     JYO-F01
         WHEN "ORDERS"
                  MOVE   LINK-IN-BDATE TO   CHK-F15
                                            ORD-F15
                  MOVE   LINK-IN-BTIME TO   CHK-F16
                                            ORD-F16
                  MOVE   LINK-IN-TORCD TO   CHK-F17
                                            ORD-F17
         WHEN "ORDCHG"
                  IF     JOH-HIT-FLG   =    "HIT"
                         MOVE  JOH-F15 TO   CHK-F15
                                            CHG-F15
                         MOVE  JOH-F16 TO   CHK-F16
                                            CHG-F16
                         MOVE  JOH-F17 TO   CHK-F17
                                            CHG-F17
                  ELSE
                         MOVE  LINK-IN-BDATE  TO   CHG-F15
                         MOVE  LINK-IN-BTIME  TO   CHG-F16
                         MOVE  LINK-IN-TORCD  TO   CHG-F17
*T
*                 DISPLAY "LINK-IN-TORCD=" LINK-IN-TORCD UPON CONS
*                 DISPLAY "CHG-F17      =" CHG-F17       UPON CONS
*T
                  END-IF
                  IF     DEN-HIT-FLG   =    "HIT"
                         MOVE  DEN-F48 TO   CHG-F18
                  END-IF
     END-EVALUATE.
*
*  基幹伝票番号・行番号
     EVALUATE     JYO-F01
         WHEN "ORDERS"
                  MOVE   ZERO          TO   CHK-F18
                                            ORD-F19
                                            CHK-F19
                                            ORD-F20
         WHEN "ORDCHG"
                  IF     JOH-HIT-FLG   =    "HIT"
                         MOVE  JOH-F18 TO   CHK-F18
                                            CHG-F19
                         MOVE  JOH-F19 TO   CHK-F19
                                            CHG-F20
                  END-IF
     END-EVALUATE.
*
*  取込担当部門ＣＤ
     MOVE         LINK-IN-BUMCD   TO   CHK-F20.
     EVALUATE     JYO-F01
         WHEN "ORDERS"
                  MOVE   LINK-IN-BUMCD TO   ORD-F21
         WHEN "ORDCHG"
                  MOVE   LINK-IN-BUMCD TO   CHG-F21
     END-EVALUATE.
*
*  取込担当者ＣＤ
     MOVE         LINK-IN-TANCD   TO   CHK-F21.
     EVALUATE     JYO-F01
         WHEN "ORDERS"
                  MOVE   LINK-IN-TANCD TO   ORD-F22
         WHEN "ORDCHG"
                  MOVE   LINK-IN-TANCD TO   CHG-F22
     END-EVALUATE.
*
*2023/04/20 NAV ST 商品名項目追加
     MOVE         JYO-F15              TO   ORD-F24  CHG-F24.
*2023/04/20 NAV ED
*    エラー区分６（発注：同一伝票存在）
     IF           JYO-F01         =   "ORDERS"
        IF        JOH-HIT-FLG     =   "INV"
                  MOVE    " "     TO   CHK-F27
        ELSE
                  MOVE    "E"     TO   CHK-F27
                  MOVE    "ER"    TO   LINK-OUT-KEKKA
        END-IF
     END-IF.
*
*    エラー区分９（発注変更：同一伝票非存在）
     IF           JYO-F01         =   "ORDCHG"
        IF        JOH-HIT-FLG     =   "HIT"
                  MOVE    " "     TO   CHK-F30
        ELSE
                  MOVE    "E"     TO   CHK-F30
                  MOVE    "ER"    TO   LINK-OUT-KEKKA
        END-IF
     END-IF.
*
*    エラー区分10（発注変更：計上済み）
     IF           JYO-F01         =   "ORDCHG"
        IF        DEN-HIT-FLG     =   "HIT"
            IF    DEN-F277    NOT =    9
                  MOVE    " "     TO   CHK-F31
            ELSE
                  MOVE    "E"     TO   CHK-F31
                  MOVE    "ER"    TO   LINK-OUT-KEKKA
            END-IF
        END-IF
     END-IF.
*
*
     WRITE     CHK-REC.
*
     IF        JYO-F01    =    "ORDERS"
               WRITE      ORD-REC
     END-IF.
*
     IF        JYO-F01    =    "ORDCHG"
               WRITE      CHG-REC
     END-IF.
*
     ADD       1             TO   WT-CNT.
*
 MAIN-ORD-99.
     READ      YODORDSF      AT   END
               MOVE  "END"   TO   END-FLG-ORD
*              CLOSE         YODORDPF
*              OPEN   INPUT  YODORDL1
               GO            TO   MAIN-ORD-EXT.
     ADD       1             TO   RD-CNT-ORD.
*
*
     GO TO   MAIN-ORD-SEC.
*
 MAIN-ORD-EXT.
     CLOSE         YODORDPF.
     OPEN   INPUT  YODORDL1.
     EXIT.
*
*----------------------------------------------------------------*
*       LEVEL     1    ﾒｲﾝ   ｼｮﾘ  ORDCHG                         *
*----------------------------------------------------------------*
*
 MAIN-CHG-SEC   SECTION.
     MOVE  "MAIN-CHG-SEC"         TO   S-NAME.
*
     IF     END-FLG-CHG = "END"
            GO                    TO   MAIN-CHG-EXT
     END-IF.
*
 MAIN-CHG-01.
*  発注伝票日付　常識チェック
     MOVE        "2"              TO   LINK-IN-KBN.
     MOVE         JYC-F03         TO   LINK-IN-YMD8.
     CALL        "SKYDTCKB"    USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD8.
*T
*    DISPLAY "LINK-OUT-RET=" LINK-OUT-RET UPON CONS.
*T
*
 MAIN-CHG-02.
*  納入期日　常識チェック
     IF         ( JYC-F14   NOT   NUMERIC ) OR
                ( JYC-F14   =     0       )
                  GO              TO   MAIN-CHG-03
     END-IF.
     MOVE        "2"              TO   LINK-IN-KBN.
     MOVE         JYC-F14         TO   LINK-IN-YMD8.
     CALL        "SKYDTCKB"    USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT2-RET
                                       LINK-OUT2-YMD8.
*T
*    DISPLAY "LINK-OUT2-RET=" LINK-OUT-RET UPON CONS.
*T
*
 MAIN-CHG-03.
*  基本情報ファイル存在チェック
     MOVE         LINK-IN-TORCD   TO   JOH-F17.
     MOVE         JYC-F03         TO   JOH-F03.
     MOVE         JYC-F02         TO   JOH-F02.
     MOVE         JYC-F07         TO   JOH-F07.
     PERFORM      JOH-READ-SEC.
*TEST
*    DISPLAY "CHG JOH-F17=" LINK-IN-TORCD UPON CONS.
*    DISPLAY "    JOH-F03=" JYC-F03 UPON CONS.
*    DISPLAY "    JOH-F02=" JYC-F02 UPON CONS.
*    DISPLAY "    JOH-F07=" JYC-F07 UPON CONS.
*    DISPLAY "    HIT-FLG=" JOH-HIT-FLG UPON CONS.
*TEST
*
*  ヨドバシ取込ワーク（発注）存在チェック
     MOVE         JYC-F03         TO   OR1-F03.
     MOVE         JYC-F02         TO   OR1-F02.
     MOVE         JYC-F07         TO   OR1-F07.
*TEST
*    DISPLAY "JYC-F03=" JYC-F03 UPON CONS.
*    DISPLAY "JYC-F02=" JYC-F02 UPON CONS.
*    DISPLAY "JYC-F07=" JYC-F07 UPON CONS.
     PERFORM      JYO-READ-SEC.
*
 MAIN-CHG-04.
*  店舗マスタ（物流センター）存在チェック（発注変更）
     IF           JYC-F01     NOT =    "ORDCHG"
                  GO              TO   MAIN-CHG-05
     END-IF.
     MOVE         LINK-IN-TORCD   TO   TEN-F52.
     MOVE         JYC-F06(1:4)    TO   WK-TEN-F011-2
     MOVE         WK-TEN-F011-RR  TO   TEN-F011.
     PERFORM      TEN-READ-SEC.
*
 MAIN-CHG-05.
*  SUB商品変換ＴＢＬ存在チェック
*    IF           JYC-F01     NOT =    "ORDERS"
*                 GO              TO   MAIN-CHG-06
*    END-IF.
     MOVE         LINK-IN-TORCD   TO   SUB-F01.
     MOVE         JYC-F08         TO   SUB-F02.
     PERFORM      SUB-READ-SEC.
*
 MAIN-CHG-06.
*  売上伝票ファイル計上済みチェック（発注変更）
     IF           JYC-F01     NOT =    "ORDCHG"
                  GO              TO   MAIN-CHG-07
     END-IF.
     IF           JOH-HIT-FLG NOT =    "HIT"
                  MOVE  "   "     TO   DEN-HIT-FLG
                  GO              TO   MAIN-CHG-07
     END-IF.
     MOVE         LINK-IN-TORCD   TO   DEN-F01.
     MOVE         JYC-F03         TO   DEN-F111.
     MOVE         JOH-F18         TO   DEN-F02.
     MOVE         JOH-F19         TO   DEN-F03.
     PERFORM      DEN-READ-SEC.
*TEST
*    DISPLAY "CHG DEN-F01 =" LINK-IN-TORCD UPON CONS.
*    DISPLAY "    DEN-F111=" JYC-F03 UPON CONS.
*    DISPLAY "    DEN-F02 =" JOH-F18 UPON CONS.
*    DISPLAY "    DEN-F03 =" JOH-F19 UPON CONS.
*    DISPLAY "    HIT-FLG =" DEN-HIT-FLG UPON CONS.
*TEST
*
*
 MAIN-CHG-07.
*  レコード編集・出力
     MOVE         SPACE           TO   ORD-REC CHG-REC CHK-REC.
     INITIALIZE                        ORD-REC CHG-REC CHK-REC.
     MOVE        ","              TO   CHK-K01 CHK-K02 CHK-K03
                                       CHK-K04 CHK-K05 CHK-K06
                                       CHK-K07 CHK-K08 CHK-K09
                                       CHK-K10 CHK-K11 CHK-K12
                                       CHK-K13 CHK-K14 CHK-K15
                                       CHK-K16 CHK-K17 CHK-K18
                                       CHK-K19 CHK-K20 CHK-K21
                                       CHK-K22 CHK-K23 CHK-K24
                                       CHK-K25 CHK-K26 CHK-K27
                                       CHK-K28 CHK-K29 CHK-K30
                                       CHK-K31 CHK-K32 CHK-K33
                                       CHK-K34 CHK-K35.
*  メッセージタイプ
     MOVE         JYC-F01         TO   CHK-F01.
     EVALUATE     JYC-F01
         WHEN "ORDERS"
                  MOVE   JYC-F01  TO   ORD-F01
         WHEN "ORDCHG"
                  MOVE   JYC-F01  TO   CHG-F01
         WHEN  OTHER
                  MOVE   "E"      TO   CHK-F22
     END-EVALUATE.
*
*  購買発注番号
     MOVE         JYC-F02         TO   CHK-F02.
     EVALUATE     JYC-F01
         WHEN "ORDERS"
                  MOVE   JYC-F02  TO   ORD-F02
         WHEN "ORDCHG"
                  MOVE   JYC-F02  TO   CHG-F02
     END-EVALUATE.
*
*  発注伝票日付
     MOVE         JYC-F03         TO   CHK-F03.
     IF           LINK-OUT-RET    =    ZERO
                  MOVE    " "     TO   CHK-F23
     ELSE
                  MOVE    "E"     TO   CHK-F23
****              MOVE    "ER"    TO   LINK-OUT-KEKKA
     END-IF.
     EVALUATE     JYC-F01
         WHEN "ORDERS"
                  MOVE   JYC-F03  TO   ORD-F03
         WHEN "ORDCHG"
                  MOVE   JYC-F03  TO   CHG-F03
     END-EVALUATE.
*
*  仕入先コード・発注先コード
     MOVE         JYC-F04         TO   CHK-F04.
     MOVE         JYC-F05         TO   CHK-F05.
     EVALUATE     JYC-F01
         WHEN "ORDERS"
                  MOVE   JYC-F04  TO   ORD-F04
                  MOVE   JYC-F05  TO   ORD-F05
         WHEN "ORDCHG"
                  MOVE   JYC-F04  TO   CHG-F04
                  MOVE   JYC-F05  TO   CHG-F05
     END-EVALUATE.
*
*  出荷先コード
     MOVE         JYC-F06         TO   CHK-F06.
     IF           TEN-HIT-FLG     =    "HIT"
                  MOVE    " "     TO   CHK-F28
     ELSE
                  MOVE    "E"     TO   CHK-F28
                  MOVE    "ER"    TO   LINK-OUT-KEKKA
     END-IF.
     EVALUATE     JYC-F01
         WHEN "ORDERS"
                  MOVE   JYC-F06  TO   ORD-F06
         WHEN "ORDCHG"
                  MOVE   JYC-F06  TO   CHG-F06
     END-EVALUATE.
*
*  明細行番号
     MOVE         JYC-F07         TO   CHK-F07.
     IF         ( JYC-F07  IS NUMERIC ) AND
                ( JYC-F07  NOT = 0    )
                  MOVE    " "     TO   CHK-F24
     ELSE
                  MOVE    "E"     TO   CHK-F24
                  MOVE    "ER"    TO   LINK-OUT-KEKKA
     END-IF.
     EVALUATE     JYC-F01
         WHEN "ORDERS"
                  MOVE   JYC-F07  TO   ORD-F07
         WHEN "ORDCHG"
                  MOVE   JYC-F07  TO   CHG-F07
     END-EVALUATE.
*
*  品目番号
     MOVE         JYC-F08         TO   CHK-F08.
     IF           SUB-HIT-FLG     =    "HIT"
                  MOVE    " "     TO   CHK-F29
     ELSE
                  MOVE    "E"     TO   CHK-F29
***               MOVE    "ER"    TO   LINK-OUT-KEKKA
                  CONTINUE
     END-IF.
     EVALUATE     JYC-F01
         WHEN "ORDERS"
                  MOVE   JYC-F08  TO   ORD-F08
         WHEN "ORDCHG"
                  MOVE   JYC-F08  TO   CHG-F08
     END-EVALUATE.
*
*  明細フリーテキスト１（キャンセル区分）
     MOVE         JYC-F09         TO   CHK-F09.
     EVALUATE     JYC-F01
         WHEN "ORDERS"
                  MOVE   JYC-F09  TO   ORD-F09
         WHEN "ORDCHG"
                  MOVE   JYC-F09  TO   CHG-F09
     END-EVALUATE.
*
*  明細単価
     MOVE         JYC-F10         TO   CHK-F10.
     IF           JYC-F01         =   "ORDCHG"
        IF        JOH-HIT-FLG     =   "HIT"
           IF     JYC-F10         =    JOH-F10
                  MOVE    " "     TO   CHK-F32
           ELSE
                  MOVE    "W"     TO   CHK-F32
                  IF       LINK-OUT-KEKKA  = "OK"
                           MOVE    "WG"    TO   LINK-OUT-KEKKA
                  END-IF
           END-IF
        END-IF
     END-IF.
     EVALUATE     JYC-F01
         WHEN "ORDERS"
                  MOVE   JYC-F10  TO   ORD-F10
         WHEN "ORDCHG"
                  MOVE   JYC-F10  TO   CHG-F10
     END-EVALUATE.
*
*  お客様伝票番号
     MOVE         JYC-F11         TO   CHK-F11.
     EVALUATE     JYC-F01
         WHEN "ORDERS"
                  MOVE   JYC-F11  TO   ORD-F11
         WHEN "ORDCHG"
                  MOVE   JYC-F11  TO   CHG-F11
     END-EVALUATE.
*
*  配送場所
     MOVE         JYC-F12         TO   CHK-F12.
     EVALUATE     JYC-F01
         WHEN "ORDERS"
                  MOVE   JYC-F12  TO   ORD-F12
         WHEN "ORDCHG"
                  MOVE   JYC-F12  TO   CHG-F12
     END-EVALUATE.
*
*  発注数量
     MOVE         JYC-F13         TO   CHK-F13.
     IF           JYC-F13  IS NUMERIC
         IF       JYC-F01  =  "ORDERS"
             IF   JYC-F13  NOT = 0
                  MOVE    " "     TO   CHK-F25
             END-IF
         END-IF
     ELSE
                  MOVE    "E"     TO   CHK-F25
                  MOVE    "ER"    TO   LINK-OUT-KEKKA
     END-IF.
     EVALUATE     JYC-F01
         WHEN "ORDERS"
                  MOVE   JYC-F13  TO   ORD-F13
         WHEN "ORDCHG"
                  MOVE   JYC-F13  TO   CHG-F13
     END-EVALUATE.
*
*  納入期日
     MOVE         JYC-F14         TO   CHK-F14.
     IF         ( JYC-F14  IS NUMERIC ) AND
                ( JYC-F14  NOT = 0    )
         IF       LINK-OUT2-RET    =    ZERO
                  MOVE    " "     TO   CHK-F26
         ELSE
                  MOVE    "E"     TO   CHK-F26
                  MOVE    "ER"    TO   LINK-OUT-KEKKA
         END-IF
     END-IF.
     EVALUATE     JYC-F01
         WHEN "ORDERS"
                  MOVE   JYC-F14  TO   ORD-F14
         WHEN "ORDCHG"
                  MOVE   JYC-F14  TO   CHG-F14
     END-EVALUATE.
*
*  バッチ（日）（時）（取）
*  倉庫
     EVALUATE     JYC-F01
         WHEN "ORDERS"
                  MOVE   LINK-IN-BDATE TO   CHK-F15
                                            ORD-F15
                  MOVE   LINK-IN-BTIME TO   CHK-F16
                                            ORD-F16
                  MOVE   LINK-IN-TORCD TO   CHK-F17
                                            ORD-F17
         WHEN "ORDCHG"
                  IF     JOH-HIT-FLG   =    "HIT"
                         MOVE  JOH-F15 TO   CHK-F15
                                            CHG-F15
                         MOVE  JOH-F16 TO   CHK-F16
                                            CHG-F16
                         MOVE  JOH-F17 TO   CHK-F17
                                            CHG-F17
                  ELSE
                         MOVE  LINK-IN-BDATE  TO   CHG-F15
                         MOVE  LINK-IN-BTIME  TO   CHG-F16
                         MOVE  LINK-IN-TORCD  TO   CHG-F17
*T
*                 DISPLAY "LINK-IN-TORCD=" LINK-IN-TORCD UPON CONS
*                 DISPLAY "CHG-F17      =" CHG-F17       UPON CONS
*T
                  END-IF
                  IF     DEN-HIT-FLG   =    "HIT"
                         MOVE  DEN-F48 TO   CHG-F18
                  END-IF
     END-EVALUATE.
*
*  基幹伝票番号・行番号
     EVALUATE     JYC-F01
         WHEN "ORDERS"
                  MOVE   ZERO          TO   CHK-F18
                                            ORD-F19
                                            CHK-F19
                                            ORD-F20
         WHEN "ORDCHG"
                  IF     JOH-HIT-FLG   =    "HIT"
                         MOVE  JOH-F18 TO   CHK-F18
                                            CHG-F19
                         MOVE  JOH-F19 TO   CHK-F19
                                            CHG-F20
                  END-IF
     END-EVALUATE.
*
*
*  取込担当部門ＣＤ
     MOVE         LINK-IN-BUMCD   TO   CHK-F20.
     EVALUATE     JYC-F01
         WHEN "ORDERS"
                  MOVE   LINK-IN-BUMCD TO   ORD-F21
         WHEN "ORDCHG"
                  MOVE   LINK-IN-BUMCD TO   CHG-F21
     END-EVALUATE.
*
*  取込担当者ＣＤ
     MOVE         LINK-IN-TANCD   TO   CHK-F21.
     EVALUATE     JYC-F01
         WHEN "ORDERS"
                  MOVE   LINK-IN-TANCD TO   ORD-F22
         WHEN "ORDCHG"
                  MOVE   LINK-IN-TANCD TO   CHG-F22
     END-EVALUATE.
*
*2023/04/20 NAV ST 商品名項目追加
     MOVE         JYO-F15              TO   ORD-F24  CHG-F24.
*2023/04/20 NAV ED
*    エラー区分６（発注：同一伝票存在）
     IF           JYC-F01         =   "ORDERS"
        IF        JOH-HIT-FLG     =   "INV"
                  MOVE    " "     TO   CHK-F27
        ELSE
                  MOVE    "E"     TO   CHK-F27
*****             MOVE    "ER"    TO   LINK-OUT-KEKKA
        END-IF
     END-IF.
*
*    エラー区分９（発注変更：同一伝票非存在）
     IF           JYC-F01         =   "ORDCHG"
        IF        JOH-HIT-FLG     =   "HIT"
                  MOVE    " "     TO   CHK-F30
        ELSE
            IF    JYO-HIT-FLG     =   "HIT"
                  MOVE    " "     TO   CHK-F30
            ELSE
                  MOVE    "E"     TO   CHK-F30
******            MOVE    "ER"    TO   LINK-OUT-KEKKA
            END-IF
        END-IF
     END-IF.
*
*    エラー区分10（発注変更：計上済み）
     IF           JYC-F01         =   "ORDCHG"
        IF        DEN-HIT-FLG     =   "HIT"
            IF    DEN-F277    NOT =    9
                  MOVE    " "     TO   CHK-F31
            ELSE
                  MOVE    "E"     TO   CHK-F31
******            MOVE    "ER"    TO   LINK-OUT-KEKKA
            END-IF
        END-IF
     END-IF.
*
*
     WRITE     CHK-REC.
*
*    IF        JYC-F01    =    "ORDERS"
*              WRITE      ORD-REC
*    END-IF.
*
     IF        JYC-F01    =    "ORDCHG"
***************2023/04/20 NAV ST
               IF  LINK-OUT-KEKKA  =  "ER"
                   MOVE  "1"           TO    CHG-F23
               END-IF
***************2023/04/20 NAV ED
               WRITE      CHG-REC
     END-IF.
*
     ADD       1             TO   WT-CNT.
*
 MAIN-CHG-99.
     READ      YODCHGSF      AT   END
               MOVE  "END"   TO   END-FLG-CHG
               GO            TO   MAIN-CHG-EXT.
     ADD       1             TO   RD-CNT-CHG.
*
*
     GO TO   MAIN-CHG-SEC.
*
 MAIN-CHG-EXT.
     EXIT.
*
****************************************************************
*　　　　　　　基本情報ファイル読込　　　　　　　　　　　　　　*
****************************************************************
 JOH-READ-SEC        SECTION.
*
     MOVE   "JOH-READ-SEC" TO        S-NAME.
*
     MOVE    "   "         TO        JOH-HIT-FLG.
     READ   YODJOHL1
       INVALID
            MOVE    "INV"  TO        JOH-HIT-FLG
       NOT  INVALID
            MOVE    "HIT"  TO        JOH-HIT-FLG
     END-READ.
*
 JOH-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ヨドバシ取込ワーク（発注）読込　　　　　　　　　*
****************************************************************
 JYO-READ-SEC        SECTION.
*
     MOVE   "JYO-READ-SEC" TO        S-NAME.
*
     MOVE    "   "         TO        JYO-HIT-FLG.
     READ   YODORDL1
       INVALID
            MOVE    "INV"  TO        JYO-HIT-FLG
       NOT  INVALID
            MOVE    "HIT"  TO        JYO-HIT-FLG
     END-READ.
*TEST
*    DISPLAY "JYO-HIT-FLG=" JYO-HIT-FLG UPON CONS.
*TEST
*
 JYO-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　店舗マスタ読込　　　　　　　　　　　　　　　　*
****************************************************************
 TEN-READ-SEC        SECTION.
*
     MOVE   "TEN-READ-SEC" TO        S-NAME.
*
     MOVE    "   "         TO        TEN-HIT-FLG.
     READ   TENMS1
       INVALID
            MOVE    "INV"  TO        TEN-HIT-FLG
       NOT  INVALID
            MOVE    "HIT"  TO        TEN-HIT-FLG
*           DISPLAY "## TENMS1 INVALID  16 , "   JYO-FA08
*                                               " ##"  UPON CONS
     END-READ.
*
 TEN-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　売上伝票Ｆ　読込　　　　　　　　　　　　　　*
****************************************************************
 DEN-READ-SEC        SECTION.
*
     MOVE   "DEN-READ-SEC" TO        S-NAME.
*
     MOVE    "   "         TO        DEN-HIT-FLG.
     READ   SHTDENLS
       INVALID
            MOVE    "INV"  TO        DEN-HIT-FLG
       NOT  INVALID
            MOVE    "HIT"  TO        DEN-HIT-FLG
*           DISPLAY "## SHTDENLS INVALID " LINK-IN-TORCD ","
*                               JYO-FA14  " ##"  UPON CONS
     END-READ.
*
 DEN-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　SUB商品変換ＴＢＬ読込
****************************************************************
 SUB-READ-SEC        SECTION.
*
     MOVE   "SUB-READ-SEC" TO        S-NAME.
*
     MOVE    "   "         TO        SUB-HIT-FLG.
     READ   SUBTBLL1
       INVALID
            MOVE    "INV"  TO        SUB-HIT-FLG
       NOT  INVALID
            MOVE    "HIT"  TO        SUB-HIT-FLG
*           DISPLAY "## SUBTBLL1 INVALID " LINK-IN-TORCD ","
*                               JYO-FA14  " ##"  UPON CONS
     END-READ.
*
 SUB-READ-EXIT.
     EXIT.
*----------------------------------------------------------------*
*       LEVEL     1    ｴﾝﾄﾞ  ｼｮﾘ                                 *
*----------------------------------------------------------------*
*
 END-SEC  SECTION.
     MOVE    "END-SEC"          TO   S-NAME.
     CLOSE   YODORDSF YODJOHL1 TENMS1 SHTDENLS SUBTBLL1
             YODCHGPF YODCHKSF.
     IF      RD-CNT-CHG    NOT =   ZERO
             CLOSE    YODORDL1
     END-IF.
     MOVE    RD-CNT-ORD    TO      IN-CNT-ORD LINK-OUT-KENSU1.
     MOVE    RD-CNT-CHG    TO      IN-CNT-CHG LINK-OUT-KENSU2.
     MOVE    WT-CNT        TO      OUT-CNT.
*    MOVE    WT-CNT        TO      LINK-OUT-KENSU1.
     DISPLAY MSG-IN-ORD    UPON CONS.
     DISPLAY MSG-IN-CHG    UPON CONS.
     DISPLAY MSG-OUT       UPON CONS.
     DISPLAY MSG-END       UPON CONS.
**
 END-EXT.
     EXIT.
 END PROGRAM NJH9400B.
