****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    業務名　　　　　　　：　ＥＤＩ　　　　　　　　　　　　　　
*    サブシステム　　　　：　ＡＭＡＺＯＮ　ＥＤＩ　　　　　　　
*    モジュール名　　　　：　AMAZON取込データ編集(受注)
*    作成日／作成者　　　：　2020/11/09 INOUE
*    処理概要　　　　　　：　ＥＯＳ名人より受け取ったデータを
*                            編集（情報付加）し、後方で処理可能
*                            な状態にする。
*    更新日／更新者　　　：　    /  /
*    更新内容　　　　　　：　　　　　　　　　　　　　　　　　　
*                            　　　　　　　　　　　　　　　　　
****************************************************************
*
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           NJH9300B.
 AUTHOR.               NAV-ASSIST.
 DATE-WRITTEN.         2020/11/09.
*
 ENVIRONMENT           DIVISION.
 CONFIGURATION         SECTION.
 SOURCE-COMPUTER.      FUJITSU.
 OBJECT-COMPUTER.      FUJITSU.
 SPECIAL-NAMES.
     CONSOLE IS        CONS.
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*アマゾン受注ワーク
     SELECT   AMZJYUW1  ASSIGN    TO    DA-01-VI-AMZJYUW1
                        ACCESS MODE    IS    SEQUENTIAL
                        ORGANIZATION   IS    INDEXED
                        RECORD KEY     IS    JYW-FA04
                                             JYW-FA03
                                             JYW-FA09
                        FILE   STATUS  IS    JYW-ST.
*アマゾン受注ファイル
     SELECT   AMZJYUPF  ASSIGN    TO   DA-01-VS-AMZJYUPF
                        ACCESS MODE    IS    SEQUENTIAL
                        ORGANIZATION   IS    SEQUENTIAL
                        FILE   STATUS  IS    JYU-ST.
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
*商品変換テーブル
     SELECT   SHOTBL1   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TBL-F01
                                            TBL-F02
                        FILE STATUS    IS   TBL-ST.
*SUB商品変換テーブル
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
*アマゾン受注ワーク
 FD  AMZJYUW1
                       LABEL RECORD   IS   STANDARD.
     COPY              AMZJYUWK       OF   XFDLIB
     JOINING           JYW            AS   PREFIX.
*
*アマゾン受注ファイル
 FD  AMZJYUPF
                       LABEL RECORD   IS   STANDARD.
     COPY              AMZJYUPF       OF   XFDLIB
     JOINING           JYU            AS   PREFIX.
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
*商品変換ＴＢＬ　
 FD  SHOTBL1
                       LABEL RECORD   IS   STANDARD.
     COPY              HSHOTBL        OF   XFDLIB
     JOINING           TBL            AS   PREFIX.
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
 77  JYW-ST                PIC  X(02)     VALUE  "00".
 77  JYU-ST                PIC  X(02)     VALUE  "00".
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
         05  ST-PG          PIC   X(08)  VALUE "NJH9300B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NJH9300B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NJH9300B".
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
   01  LINK-IN-BDATE              PIC 9(08).
   01  LINK-IN-BTIME              PIC 9(04).
   01  LINK-IN-BUMCD              PIC X(04).
   01  LINK-IN-TANCD              PIC X(02).
   01  LINK-IN-TORCD              PIC 9(08).
   01  LINK-OUT-KENSU             PIC 9(07).
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
                                         LINK-OUT-KENSU
                                         LINK-OUT-KEKKA.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   AMZJYUW1.
     MOVE      "AMZJYUW1"   TO   AB-FILE.
     MOVE      JYW-ST       TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     MOVE     "ER"          TO   LINK-OUT-KEKKA.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   AMZJYUPF.
     MOVE      "AMZJYUPF"   TO   AB-FILE.
     MOVE      JYU-ST       TO   AB-STS.
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
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHOTBL1.
     MOVE      "SHOTBL1 "   TO   AB-FILE.
     MOVE      TBL-ST       TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     MOVE     "ER"          TO   LINK-OUT-KEKKA.
     STOP      RUN.
*
 FILEERR-SEC6           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SUBTBLL1.
     MOVE      "SUBTBLL1"   TO   AB-FILE.
     MOVE      SUB-ST       TO   AB-STS.
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
     OPEN    INPUT   AMZJYUW1 AMZJOHL1 JYOKEN1 SHOTBL1 SUBTBLL1.
     OPEN    OUTPUT  AMZJYUPF.
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
     READ    AMZJYUW1
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
     IF      JYW-F001  NOT = "1"
             DISPLAY   NC"＃＃＃＃＃＃＃＃＃＃＃＃＃" UPON CONS
             DISPLAY   NC"＃＃　受注データでは　＃＃" UPON CONS
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
     MOVE         JYW-FA04        TO   JOH-FA04.
     MOVE         JYW-FA03        TO   JOH-FA03.
     MOVE         JYW-FA09        TO   JOH-FA09.
     PERFORM      JOH-READ-SEC.
*
 MAIN-02.
*  条件ファイル（店舗コード）存在チェック
     MOVE         26              TO   JYO-F01.
     MOVE         JYW-FA08        TO   JYO-F02.
     PERFORM      JYO-READ-SEC.
*
 MAIN-03.
*  商品変換ＴＢＬ・SUB商品変換ＴＢＬ存在チェック
     MOVE         LINK-IN-TORCD   TO   TBL-F01 SUB-F01.
     MOVE         JYW-FA14        TO   TBL-F02 SUB-F02.
     PERFORM      TBL-READ-SEC.
     PERFORM      SUB-READ-SEC.
*
 MAIN-04.
*  発注日付　常識チェック
     MOVE        "2"              TO   LINK-IN-KBN.
     MOVE         JYW-FA04        TO   LINK-IN-YMD8.
     CALL        "SKYDTCKB"    USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD8.
*T
*    DISPLAY "LINK-OUT-RET=" LINK-OUT-RET UPON CONS.
*T
*
 MAIN-05.
*  レコード編集・出力
     MOVE         SPACE           TO   JYU-REC.
     INITIALIZE                        JYU-REC.
*  【共通情報】F0
     MOVE         JYW-F0          TO   JYU-F0.
*  【受注情報】FA
     MOVE         JYW-FA          TO   JYU-FA.
*  【引当情報】FB ※先行セット
*    ロケーションコード　　
     IF           TBL-HIT-FLG     =    "HIT"
          MOVE    TBL-F08         TO   JYU-FB10
     END-IF.
*    店番
     IF           JYO-HIT-FLG     =    "HIT"
          MOVE    JYO-F04         TO   JYU-FB11
     END-IF.
*    ケース入数
     IF           SUB-HIT-FLG     =    "HIT"
          MOVE    SUB-F15         TO   JYU-FB13
     END-IF.
*  【検品実績】FC
*
*  【予備】    FD
*
*  【付加情報】FE
*    バッチ（日付）
     MOVE         LINK-IN-BDATE   TO   JYU-FE01.
*    バッチ（時刻）
     MOVE         LINK-IN-BTIME   TO   JYU-FE02.
*    バッチ（取Ｃ）
     MOVE         LINK-IN-TORCD   TO   JYU-FE03.
*    受注  （日付）
     MOVE         SYS-DATEW       TO   JYU-FE04.
*    受注  （時刻）
     MOVE         WK-TIME(1:6)    TO   JYU-FE05.
*    受注  （部Ｃ）
     MOVE         LINK-IN-BUMCD   TO   JYU-FE06.
*    受注  （担Ｃ）
     MOVE         LINK-IN-TANCD   TO   JYU-FE07.
*    エラー１（受注）
*    受注データ重複（基本情報ファイルあり）
     IF           JOH-HIT-FLG     =    "HIT"
          MOVE    "1"             TO   JYU-FE08
          MOVE    "1"             TO   JYU-FE081
          MOVE    "ER"            TO   LINK-OUT-KEKKA
     ELSE
          MOVE    " "             TO   JYU-FE081
     END-IF.
*    エラー２（受注）
*    店番変換不可　（条件ファイルなし）
     IF           JYO-HIT-FLG     =    "HIT"
          MOVE    " "             TO   JYU-FE082
     ELSE
          MOVE    "1"             TO   JYU-FE08
          MOVE    "1"             TO   JYU-FE082
          MOVE    "ER"            TO   LINK-OUT-KEKKA
     END-IF.
*    エラー３（受注）
*    ロケーション・入数取得不可（変換ＴＢＬなし）
     IF         ( TBL-HIT-FLG     =    "HIT" )  AND
                ( SUB-HIT-FLG     =    "HIT" )
          MOVE    " "             TO   JYU-FE083
     ELSE
          MOVE    "1"             TO   JYU-FE08
          MOVE    "1"             TO   JYU-FE083
          MOVE    "ER"            TO   LINK-OUT-KEKKA
     END-IF.
*    エラー４（受注）
*    発注日異常（常識チェック）
     IF   LINK-OUT-RET   =    ZERO
          MOVE    " "             TO   JYU-FE084
     ELSE
          MOVE    "1"             TO   JYU-FE08
          MOVE    "1"             TO   JYU-FE084
          MOVE    "ER"            TO   LINK-OUT-KEKKA
     END-IF.
*
     WRITE     JYU-REC.
     ADD       1             TO   WT-CNT.
*
 MAIN-99.
     READ      AMZJYUW1      AT   END
               GO            TO   MAIN-EXT.
     ADD       1             TO   RD-CNT.
*
     IF      JYW-F001 NOT = "1"
             DISPLAY  NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃" UPON CONS
             DISPLAY  NC"＃＃　受注データ以外が　＃＃" UPON CONS
             DISPLAY  NC"＃＃　混在しています！　＃＃" UPON CONS
             DISPLAY  NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃" UPON CONS
*            MOVE     4010       TO      PROGRAM-STATUS
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
*           DISPLAY "## JYOKEN1 INVALID  16 , "   JYW-FA08
*                                               " ##"  UPON CONS
     END-READ.
*
 JYO-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　商品変換ＴＢＬ　読込　　　　　　　　　　　　　　*
****************************************************************
 TBL-READ-SEC        SECTION.
*
     MOVE   "TBL-READ-SEC" TO        S-NAME.
*
     READ   SHOTBL1
       INVALID
            MOVE    "INV"  TO        TBL-HIT-FLG
*******     DISPLAY "TBL-F02 ERR = " TBL-F02 UPON CONS
       NOT  INVALID
            MOVE    "HIT"  TO        TBL-HIT-FLG
*           DISPLAY "## SHOTBL1 INVALID " LINK-IN-TORCD ","
*                               JYW-FA14  " ##"  UPON CONS
     END-READ.
*
 TBL-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　SUB商品変換ＴＢＬ読込
****************************************************************
 SUB-READ-SEC        SECTION.
*
     MOVE   "SUB-READ-SEC" TO        S-NAME.
*
     READ   SUBTBLL1
       INVALID
            MOVE    "INV"  TO        SUB-HIT-FLG
       NOT  INVALID
            MOVE    "HIT"  TO        SUB-HIT-FLG
*           DISPLAY "## SUBTBLL1 INVALID " LINK-IN-TORCD ","
*                               JYW-FA14  " ##"  UPON CONS
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
     CLOSE   AMZJYUW1 AMZJOHL1 JYOKEN1 SHOTBL1 SUBTBLL1
             AMZJYUPF.
     MOVE    RD-CNT    TO      IN-CNT.
     MOVE    WT-CNT    TO      OUT-CNT
                               LINK-OUT-KENSU.
     DISPLAY MSG-IN    UPON CONS.
     DISPLAY MSG-OUT   UPON CONS.
     DISPLAY MSG-END   UPON CONS.
**
 END-EXT.
     EXIT.
 END PROGRAM NJH9300B.
