****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    業務名　　　　　　　：　ＥＤＩ　　　　　　　　　　　　　　
*    サブシステム　　　　：　ＡＭＡＺＯＮ　ＥＤＩ　　　　　　　
*    モジュール名　　　　：　AMAZON取込データ編集(検品)
*    作成日／作成者　　　：　2020/11/13 INOUE
*    処理概要　　　　　　：　ＥＯＳ名人より受け取ったデータを
*                            編集（情報付加）し、後方で処理可能
*                            な状態にする。
*    更新日／更新者　　　：　    /  /
*    更新内容　　　　　　：　　　　　　　　　　　　　　　　　　
*                            　　　　　　　　　　　　　　　　　
****************************************************************
*
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           NJH9306B.
 AUTHOR.               NAV-ASSIST.
 DATE-WRITTEN.         2020/11/13.
*
 ENVIRONMENT           DIVISION.
 CONFIGURATION         SECTION.
 SOURCE-COMPUTER.      FUJITSU.
 OBJECT-COMPUTER.      FUJITSU.
 SPECIAL-NAMES.
     CONSOLE IS        CONS.
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*アマゾン出荷確定ワーク
     SELECT   AMZSYKW1  ASSIGN    TO    DA-01-VI-AMZSYKW1
                        ACCESS MODE    IS    SEQUENTIAL
                        ORGANIZATION   IS    INDEXED
                        RECORD KEY     IS    SYW-FA04
                                             SYW-FA03
                                             SYW-FA09
                        FILE   STATUS  IS    SYW-ST.
*アマゾン出荷確定ファイル
     SELECT   AMZSYKPF  ASSIGN    TO   DA-01-VS-AMZSYKPF
                        ACCESS MODE    IS    SEQUENTIAL
                        ORGANIZATION   IS    SEQUENTIAL
                        FILE   STATUS  IS    SYK-ST.
*アマゾン基本情報ファイル
     SELECT   AMZJOHL1  ASSIGN    TO        DA-01-VI-AMZJOHL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JOH-FA04
                                            JOH-FA03
                                            JOH-FA09
                        FILE      STATUS    JOH-ST.
*条件ファイル
     SELECT   JYOKEN1   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       JYO-F01
                                            JYO-F02
                        FILE      STATUS    JYO-ST.
******************************************************************
*                                                                *
*    DATA              DIVISION                                  *
*                                                                *
******************************************************************
*
 DATA                  DIVISION.
 FILE                  SECTION.
*アマゾン出荷確定ワーク
 FD  AMZSYKW1
                       LABEL RECORD   IS   STANDARD.
     COPY              AMZSYKWK       OF   XFDLIB
     JOINING           SYW            AS   PREFIX.
*
*アマゾン出荷確定ファイル
 FD  AMZSYKPF
                       LABEL RECORD   IS   STANDARD.
     COPY              AMZSYKPF       OF   XFDLIB
     JOINING           SYK            AS   PREFIX.
*
*アマゾン基本情報ファイル
 FD  AMZJOHL1
                       LABEL RECORD   IS   STANDARD.
     COPY              AMZJOHPF       OF   XFDLIB
     JOINING           JOH            AS   PREFIX.
*
*条件ファイル
 FD  JYOKEN1
                       LABEL RECORD   IS   STANDARD.
     COPY              HJYOKEN        OF   XFDLIB
     JOINING           JYO            AS   PREFIX.
*
******************************************************************
*                 WORKING-STORAGE   SECTION
******************************************************************
 WORKING-STORAGE       SECTION.
*
 77  SYW-ST                PIC  X(02)     VALUE  "00".
 77  SYK-ST                PIC  X(02)     VALUE  "00".
 77  JOH-ST                PIC  X(02)     VALUE  "00".
 77  JYO-ST                PIC  X(02)     VALUE  "00".
 77  TBL-ST                PIC  X(02)     VALUE  "00".
 77  SUB-ST                PIC  X(02)     VALUE  "00".
 77  END-FLG               PIC  X         VALUE  " ".
 01  RD-CNT                PIC  9(07)     VALUE  ZERO.
 01  WT-CNT                PIC  9(07)     VALUE  ZERO.
 01  OK-FLG                PIC  X(02)     VALUE  "  ".
 01  JOH-HIT-FLG           PIC  X(03)     VALUE  "   ".
 01  JYO-HIT-FLG           PIC  X(03)     VALUE  "   ".
 01  TBL-HIT-FLG           PIC  X(03)     VALUE  "   ".
 01  SUB-HIT-FLG           PIC  X(03)     VALUE  "   ".
 01  FLG-DATECHK1          PIC  X(02)     VALUE  "  ".
 01  FLG-DATECHK2          PIC  X(02)     VALUE  "  ".
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
         05  ST-PG          PIC   X(08)  VALUE "NJH9306B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NJH9306B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NJH9306B".
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
         05  IN-CNT         PIC   9(07).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT        PIC   9(07).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
 LINKAGE SECTION.
   01  LINK-IN-BUMCD              PIC X(04).
   01  LINK-IN-TANCD              PIC X(02).
   01  LINK-OUT-KENSU             PIC 9(07).
   01  LINK-OUT-KEKKA             PIC X(02).
******************************************************************
*                                                                *
*    PROCEDURE         DIVISION                                  *
*                                                                *
******************************************************************
*
 PROCEDURE             DIVISION   USING
                                         LINK-IN-BUMCD
                                         LINK-IN-TANCD
                                         LINK-OUT-KENSU
                                         LINK-OUT-KEKKA.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   AMZSYKW1.
     MOVE      "AMZSYKW1"   TO   AB-FILE.
     MOVE      SYW-ST       TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     MOVE     "ER"          TO   LINK-OUT-KEKKA.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   AMZSYKPF.
     MOVE      "AMZSYKPF"   TO   AB-FILE.
     MOVE      SYK-ST       TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     MOVE     "ER"          TO   LINK-OUT-KEKKA.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   AMZJOHL1.
     MOVE      "AMZJOHL1"   TO   AB-FILE.
     MOVE      JOH-ST       TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     MOVE     "ER"          TO   LINK-OUT-KEKKA.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JYOKEN1.
     MOVE      "JYOKEN1 "   TO   AB-FILE.
     MOVE      JYO-ST       TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     MOVE     "ER"          TO   LINK-OUT-KEKKA.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
 PRG-CONTROL  SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM      INIT-SEC.
     PERFORM      MAIN-SEC.
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
     OPEN    INPUT   AMZSYKW1 AMZJOHL1 JYOKEN1.
     OPEN    OUTPUT  AMZSYKPF.
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
 INIT-2.
     READ    AMZSYKW1
       AT END
             DISPLAY   NC"＃＃＃＃＃＃＃＃＃＃＃＃＃" UPON CONS
             DISPLAY   NC"＃＃　データ０件です　＃＃" UPON CONS
             DISPLAY   NC"＃＃＃＃＃＃＃＃＃＃＃＃＃" UPON CONS
*            MOVE      4010       TO      PROGRAM-STATUS
             MOVE     "ER"        TO      LINK-OUT-KEKKA
             STOP      RUN
     END-READ.
*
 INIT-3.
     IF      SYW-F001  NOT = "3"
             DISPLAY   NC"＃＃＃＃＃＃＃＃＃＃＃＃＃" UPON CONS
             DISPLAY   NC"＃＃　検品データでは　＃＃" UPON CONS
             DISPLAY   NC"＃＃　ありません！！　＃＃" UPON CONS
             DISPLAY   NC"＃＃＃＃＃＃＃＃＃＃＃＃＃" UPON CONS
*            MOVE      4010       TO      PROGRAM-STATUS
             MOVE     "ER"        TO      LINK-OUT-KEKKA
             STOP      RUN
     END-IF.
*
 INIT-4.
*    MOVE    IN-REC               TO      WK-HEAD.
     MOVE    "OK"                 TO      LINK-OUT-KEKKA.
     ADD     1                    TO      RD-CNT.
*
 INIT-EXT.
     EXIT.
*
*----------------------------------------------------------------*
*       LEVEL     1    ﾒｲﾝ   ｼｮﾘ                                 *
*----------------------------------------------------------------*
*
 MAIN-SEC   SECTION.
     MOVE  "MAIN-SEC"             TO   S-NAME.
*
*MAIN-01.
*  基本情報ファイル存在チェック
     MOVE         SYW-FA04        TO   JOH-FA04.
     MOVE         SYW-FA03        TO   JOH-FA03.
     MOVE         SYW-FA09        TO   JOH-FA09.
     PERFORM      JOH-READ-SEC.
*
 MAIN-02.
*  条件ファイル（店舗コード）存在チェック
     MOVE         26              TO   JYO-F01.
     MOVE         SYW-FA08        TO   JYO-F02.
     PERFORM      JYO-READ-SEC.
*
 MAIN-03.
*  商品変換ＴＢＬ・SUB商品変換ＴＢＬ存在チェック
*    MOVE         LINK-IN-TORCD   TO   TBL-F01 SUB-F01.
*    MOVE         SYW-FA14        TO   TBL-F02 SUB-F02.
*    PERFORM      TBL-READ-SEC.
*    PERFORM      SUB-READ-SEC.
*
 MAIN-04.
*  出荷日付　常識チェック
     MOVE        "2"              TO   LINK-IN-KBN.
     MOVE         SYW-FC03        TO   LINK-IN-YMD8.
     CALL        "SKYDTCKB"    USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD8.
     IF           LINK-OUT-RET   =    ZERO
         MOVE     "OK"           TO   FLG-DATECHK1
     ELSE
         MOVE     "NG"           TO   FLG-DATECHK1
     END-IF.
*T
*    DISPLAY "LINK-OUT-RET=" LINK-OUT-RET UPON CONS.
*T
*
 MAIN-05.
*  レコード編集・出力
     MOVE         SPACE           TO   SYK-REC.
     INITIALIZE                        SYK-REC.
*  【共通情報】F0
     MOVE         SYW-F0          TO   SYK-F0.
*  【受注情報】FA
     MOVE         SYW-FA          TO   SYK-FA.
*  【引当情報】FB
     MOVE         SYW-FB          TO   SYK-FB.
*  【検品実績】FC
     MOVE         SYW-FC          TO   SYK-FC.
*
*  【予備】    FD
*
*  【付加情報】FE
     IF           JOH-HIT-FLG     =     "HIT"
                  MOVE   JOH-FE          TO   SYK-FE
     END-IF.
*----確定（日付）（時刻）は後方処理で更新する
*----確定  （日付）
*----MOVE         SYS-DATEW       TO   SYK-FE14.
*----確定  （時刻）
*----MOVE         WK-TIME(1:6)    TO   SYK-FE15.
*    確定  （部Ｃ）
     MOVE         LINK-IN-BUMCD   TO   SYK-FE16.
*    確定  （担Ｃ）
     MOVE         LINK-IN-TANCD   TO   SYK-FE17.
*
     IF        (  JYO-HIT-FLG     =     "HIT"  )
          AND  (  FLG-DATECHK1    =     "OK"   )
*    納品日  出荷日+N
                  MOVE   "5"             TO   LINK-IN-KBN
                  MOVE    JYO-F05        TO   LINK-IN-YMD6
                  MOVE    SYW-FC03       TO   LINK-IN-YMD8
                  CALL   "SKYDTCKB"   USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD8
                  IF      LINK-OUT-RET   =    ZERO
                          MOVE     "OK"       TO   FLG-DATECHK2
                          MOVE LINK-OUT-YMD8  TO   SYK-FE18
                  ELSE
                          MOVE     "NG"       TO   FLG-DATECHK2
*--                       MOVE     ZERO       TO   SYK-FE18
                  END-IF
     ELSE
                  MOVE   "NG"            TO   FLG-DATECHK2
     END-IF.
*
*    エラー１（確定）
*    確定データ重複（基本情報ファイルなし）
     IF           JOH-HIT-FLG     =    "INV"
          MOVE    "1"             TO   SYK-FE19
          MOVE    "1"             TO   SYK-FE191
          MOVE    "ER"            TO   LINK-OUT-KEKKA
     ELSE
          MOVE    " "             TO   SYK-FE191
     END-IF.
*    エラー２（確定）
*    数量異常　　　（指示数＜出荷確定数）
     IF           JOH-HIT-FLG     =    "HIT"
          IF      JOH-FB04        >=   SYW-FC05
                  MOVE    " "     TO   SYK-FE192
          ELSE
                  MOVE    "1"     TO   SYK-FE19
                  MOVE    "1"     TO   SYK-FE192
                  MOVE    "ER"    TO   LINK-OUT-KEKKA
          END-IF
     END-IF.
*    エラー３（確定）
*    出荷日異常 OR 算出不可（条件ファイルなし）
     IF         ( FLG-DATECHK1    =    "OK" )  AND
                ( FLG-DATECHK2    =    "OK" )
          MOVE    " "             TO   SYK-FE193
     ELSE
          MOVE    "1"             TO   SYK-FE19
          MOVE    "1"             TO   SYK-FE193
          MOVE    "ER"            TO   LINK-OUT-KEKKA
     END-IF.
*    エラー４（確定）
*    更新（基本情報F・売上伝票F・在庫M反映済）
     IF        JOH-HIT-FLG    =    "HIT"
          IF   JOH-F001       =    "2"
               MOVE    " "             TO   SYK-FE194
          ELSE
               MOVE    "1"             TO   SYK-FE19
               MOVE    "1"             TO   SYK-FE194
               MOVE    "ER"            TO   LINK-OUT-KEKKA
          END-IF
     END-IF.
*
     WRITE     SYK-REC.
     ADD       1             TO   WT-CNT.
*
 MAIN-99.
     READ      AMZSYKW1      AT   END
               GO            TO   MAIN-EXT.
     ADD       1             TO   RD-CNT.
*
     IF      SYW-F001 NOT = "3"
             DISPLAY  NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃" UPON CONS
             DISPLAY  NC"＃＃　検品データ以外が　＃＃" UPON CONS
             DISPLAY  NC"＃＃　混在しています！　＃＃" UPON CONS
             DISPLAY  NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃" UPON CONS
*            MOVE     4010       TO      PROGRAM-STATUS
             MOVE     WT-CNT     TO      LINK-OUT-KENSU
             MOVE    "ER"        TO      LINK-OUT-KEKKA
             STOP     RUN
     END-IF.
*
     GO TO   MAIN-SEC.
*
 MAIN-EXT.
     EXIT.
*
****************************************************************
*　　　　　　　基本情報ファイル読込　　　　　　　　　　　　　　*
****************************************************************
 JOH-READ-SEC        SECTION.
*
     MOVE   "JOH-READ-SEC" TO        S-NAME.
*
     READ   AMZJOHL1
       INVALID
            MOVE    "INV"  TO        JOH-HIT-FLG
       NOT  INVALID
            MOVE    "HIT"  TO        JOH-HIT-FLG
     END-READ.
*
 JOH-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　条件ファイル読込　　　　　　　　　　　　　　　　*
****************************************************************
 JYO-READ-SEC        SECTION.
*
     MOVE   "JYO-READ-SEC" TO        S-NAME.
*
     READ   JYOKEN1
       INVALID
            MOVE    "INV"  TO        JYO-HIT-FLG
       NOT  INVALID
            MOVE    "HIT"  TO        JYO-HIT-FLG
*           DISPLAY "## JYOKEN1 INVALID  16 , "   SYW-FA08
*                                               " ##"  UPON CONS
     END-READ.
*
 JYO-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　商品変換ＴＢＬ　読込　　　　　　　　　　　　　　*
****************************************************************
*TBL-READ-SEC        SECTION.
*
*    MOVE   "TBL-READ-SEC" TO        S-NAME.
*
*    READ   SHOTBL1
*      INVALID
*           MOVE    "INV"  TO        TBL-HIT-FLG
*      NOT  INVALID
*           MOVE    "HIT"  TO        TBL-HIT-FLG
*           DISPLAY "## SHOTBL1 INVALID " LINK-IN-TORCD ","
*                               SYW-FA14  " ##"  UPON CONS
*    END-READ.
*
*TBL-READ-EXIT.
*    EXIT.
****************************************************************
*　　　　　　　SUB商品変換ＴＢＬ読込
****************************************************************
*SUB-READ-SEC        SECTION.
*
*    MOVE   "SUB-READ-SEC" TO        S-NAME.
*
*    READ   SUBTBLL1
*      INVALID
*           MOVE    "INV"  TO        SUB-HIT-FLG
*      NOT  INVALID
*           MOVE    "HIT"  TO        SUB-HIT-FLG
*           DISPLAY "## SUBTBLL1 INVALID " LINK-IN-TORCD ","
*                               SYW-FA14  " ##"  UPON CONS
*    END-READ.
*
*SUB-READ-EXIT.
*    EXIT.
*----------------------------------------------------------------*
*       LEVEL     1    ｴﾝﾄﾞ  ｼｮﾘ                                 *
*----------------------------------------------------------------*
*
 END-SEC  SECTION.
     MOVE    "END-SEC"          TO   S-NAME.
     CLOSE   AMZSYKW1 AMZJOHL1 JYOKEN1 AMZSYKPF.
*
     MOVE    RD-CNT    TO      IN-CNT.
     MOVE    WT-CNT    TO      OUT-CNT
                               LINK-OUT-KENSU.
     DISPLAY MSG-IN    UPON CONS.
     DISPLAY MSG-OUT   UPON CONS.
     DISPLAY MSG-END   UPON CONS.
**
 END-EXT.
     EXIT.
 END PROGRAM NJH9306B.
