# SSY9302B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SSY9302B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　出荷　　　　　　　　　　　　　　  *
*    サブシステム　　　　：　ＡＭＡＺＯＮ　ＥＤＩ　　　　　　  *
*    モジュール名　　　　：　ＡＭＡＺＯＮ出荷予定データ作成    *
*    作成日／作成者　　　：　2020/11/12 INOUE                  *
*    処理概要　　　　　　：　受け取ったパラメタに合致する　　  *
*                            出荷予定データを作成する。　　　　*
*    更新履歴                                                  *
*    更新日／更新者　　　：　                                  *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY9302B.
*                      :SSY8723B
 AUTHOR.                NAV.
 DATE-WRITTEN.          2020/11/12.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< ＡＭＡＺＯＮ基本情報ファイル >>----*
     SELECT   AMZJOHL2  ASSIGN              DA-01-VI-AMZJOHL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       JOH-FE01  JOH-FE02
                                            JOH-FE03  JOH-FB09
                                            JOH-FA04
                        FILE      STATUS    JOH-STATUS.
*----<< 売上伝票ファイル >>----*
     SELECT   SHTDENL1  ASSIGN    TO        DA-01-VI-SHTDENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       DEN-F01   DEN-F02
                                            DEN-F04   DEN-F051
                                            DEN-F07   DEN-F112
                                            DEN-F03
                        FILE      STATUS    DEN-STATUS.
*----<< 条件ファイル >>----*
*    SELECT   JYOKEN1   ASSIGN    TO        DA-01-VI-JYOKEN1
*                       ORGANIZATION        INDEXED
*                       ACCESS    MODE      DYNAMIC
*                       RECORD    KEY       JYO-F01
*                                           JYO-F02
*                       FILE      STATUS    JYO-STATUS.
*----<< ＡＭＡＺＯＮ出荷予定ワーク >>----*
     SELECT   AMZSYYWK   ASSIGN    TO       DA-01-VS-AMZSYYWK
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    SYY-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    ＡＭＡＺＯＮ基本情報ファイル
******************************************************************
 FD  AMZJOHL2            LABEL RECORD   IS   STANDARD.
     COPY     AMZJOHL2   OF       XFDLIB
              JOINING   JOH       PREFIX.
*
******************************************************************
*    売上伝票ファイル
******************************************************************
 FD  SHTDENL1
                        LABEL RECORD   IS   STANDARD.
     COPY     SHTDENL1  OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
*
******************************************************************
*    条件ファイル
******************************************************************
*FD  JYOKEN1
*                       LABEL RECORD   IS   STANDARD.
*    COPY     JYOKEN1   OF        XFDLIB
*             JOINING   JYO  AS   PREFIX.
*
******************************************************************
*    ＡＭＡＺＯＮ発注出荷予定ワーク
******************************************************************
 FD  AMZSYYWK            LABEL RECORD   IS   STANDARD.
     COPY     AMZSYYWK   OF        XFDLIB
              JOINING    SYY       PREFIX.
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
     03  AMZSYYWK-INV-FLG    PIC  X(03)     VALUE  SPACE.
     03  SHTDENL1-INV-FLG    PIC  X(03)     VALUE  SPACE.
 01  WK-GYO-CNT              PIC  9(02)     VALUE  ZERO.
 01  WK-KMS-F06              PIC  9(09)     VALUE  ZERO.
*状態区分一時保管用
 01  JOUTAI                  PIC  X(02)     VALUE  SPACE.
*消費税率取得サブ用
 01  WK-ZEIHENKAN.
     03  LINK-ZEIKBN              PIC  X(01).
     03  LINK-ZEIDATE             PIC  9(08).
     03  LINK-ZEIERR              PIC  X(01).
     03  LINK-ZEIRITU             PIC  9(05).
     03  LINK-DZEIRITU            PIC  9(05).
     03  LINK-ZEIRITU-H           PIC  9(03)V9(02).
     03  LINK-DZEIRITU-H          PIC  9(03)V9(02).
 01  ZEIRITU                      PIC  9(03)V9(02)  VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME           PIC   9(08)  VALUE  ZERO.
*
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  JOH-STATUS        PIC  X(02).
     03  SYY-STATUS        PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY9302B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY9302B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY9302B".
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
     03  FILLER            PIC  X(02)   VALUE  SPACE.
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
 01  PARA-IN-JDATE             PIC   9(08).
 01  PARA-IN-JTIME             PIC   9(04).
 01  PARA-IN-TORICD            PIC   9(08).
 01  PARA-IN-SOKO              PIC   X(02).
 01  PARA-IN-NOUDT             PIC   9(08).
 01  PARA-IN-BUMCD             PIC   X(04).
 01  PARA-IN-TANCD             PIC   X(02).
 01  PARA-OUT-KENSU            PIC   9(07).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-IN-JDATE
                                       PARA-IN-JTIME
                                       PARA-IN-TORICD
                                       PARA-IN-SOKO
                                       PARA-IN-NOUDT
                                       PARA-IN-BUMCD
                                       PARA-IN-TANCD
                                       PARA-OUT-KENSU.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   AMZJOHL2.
     MOVE      "AMZJOHL2 "   TO   AB-FILE.
     MOVE      JOH-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHTDENL1.
     MOVE      "SHTDENL1"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
*FILEERR-SEC3           SECTION.
*    USE       AFTER    EXCEPTION
*                       PROCEDURE   JYOKEN1.
*    MOVE      "JYOKEN1 "   TO   AB-FILE.
*    MOVE      JYO-STATUS   TO   AB-STS.
*    DISPLAY   MSG-ABEND         UPON CONS.
*    DISPLAY   SEC-NAME          UPON CONS.
*    DISPLAY   ABEND-FILE        UPON CONS.
*    MOVE      4000         TO   PROGRAM-STATUS.
*    STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   AMZSYYWK.
     MOVE      "AMZSYYWK"   TO   AB-FILE.
     MOVE      SYY-STATUS   TO   AB-STS.
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
     OPEN     I-O       AMZJOHL2.
*    OPEN     INPUT     SHTDENL1  JYOKEN1.
     OPEN     INPUT     SHTDENL1.
     OPEN     OUTPUT    AMZSYYWK.
*
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
*
*   システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*
*    ＡＭＡＺＯＮ基本情報ファイルスタート
     MOVE     SPACE          TO   JOH-REC.
     INITIALIZE                   JOH-REC.
     MOVE     PARA-IN-JDATE  TO   JOH-FE01.
     MOVE     PARA-IN-JTIME  TO   JOH-FE02.
     MOVE     PARA-IN-TORICD TO   JOH-FE03.
     MOVE     PARA-IN-SOKO   TO   JOH-FB09.
     MOVE     PARA-IN-NOUDT  TO   JOH-FA04.
     START    AMZJOHL2  KEY  >=   JOH-FE01   JOH-FE02
                                  JOH-FE03   JOH-FB09
                                  JOH-FA04
         INVALID   KEY
              MOVE    "END"  TO   END-FLG
              GO             TO   INIT-EXIT
     END-START.
*
*    ＡＭＡＺＯＮ基本情報ファイル読込み
     PERFORM AMZJOHL2-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 AMZJOHL2-READ-SEC    SECTION.
*
     MOVE    "AMZJOHL2-READ-SEC"    TO  S-NAME.
*
     READ     AMZJOHL2
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  AMZJOHL2-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*    バッチ番号のチェック
     IF       PARA-IN-JDATE  =  JOH-FE01
     AND      PARA-IN-JTIME  =  JOH-FE02
     AND      PARA-IN-TORICD =  JOH-FE03
              CONTINUE
     ELSE
              MOVE     "END"        TO  END-FLG
              GO                    TO  AMZJOHL2-READ-EXIT
     END-IF.
*    倉庫ＣＤチェック
     IF       PARA-IN-SOKO   =  SPACE
              CONTINUE
     ELSE
              IF   PARA-IN-SOKO  =  JOH-FB09
                   CONTINUE
              ELSE
                   GO            TO  AMZJOHL2-READ-SEC
              END-IF
     END-IF.
*    納品日のチェック
     IF       PARA-IN-NOUDT  =  ZERO
              CONTINUE
     ELSE
              IF  PARA-IN-NOUDT  =  JOH-FA04
                  CONTINUE
              ELSE
                  GO        TO   AMZJOHL2-READ-SEC
              END-IF
     END-IF.
*
 AMZJOHL2-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*
*    売上伝票ファイル検索
     MOVE     JOH-FE03            TO   DEN-F01.  *> 取引先ＣＤ
     MOVE     JOH-FB01            TO   DEN-F02.  *> 伝票番号
     MOVE     0                   TO   DEN-F04.  *> 相殺区分
     MOVE     40                  TO   DEN-F051. *> 伝票区分
     MOVE     JOH-FB11            TO   DEN-F07.  *> 店舗CD
     MOVE     JOH-FA04            TO   DEN-F112. *> 納品日
     MOVE     JOH-FB02            TO   DEN-F03.  *> 行
*T
*    DISPLAY "DEN-F01  = "  DEN-F01  UPON CONS.
*    DISPLAY "DEN-F02  = "  DEN-F02  UPON CONS.
*    DISPLAY "DEN-F04  = "  DEN-F04  UPON CONS.
*    DISPLAY "DEN-F051 = "  DEN-F051 UPON CONS.
*    DISPLAY "DEN-F07  = "  DEN-F07  UPON CONS.
*    DISPLAY "DEN-F112 = "  DEN-F112 UPON CONS.
*    DISPLAY "DEN-F03  = "  DEN-F03  UPON CONS.
*T
     READ     SHTDENL1    INVALID
              MOVE    "INV"       TO   SHTDENL1-INV-FLG
              NOT  INVALID
              MOVE    SPACE       TO   SHTDENL1-INV-FLG
     END-READ.
*
     IF      SHTDENL1-INV-FLG  =  "INV"
*            対象外
              ADD     1             TO   SKIP2-CNT
              GO                    TO   MAIN010
     ELSE
*            データ作成
              PERFORM   AMZSYYWK-WRITE-SEC
*            共通情報_データ区分
              MOVE   "2"            TO   JOH-F001
*            引当情報_状態区分
              MOVE   JOUTAI         TO   JOH-FB03
              MOVE   SPACE          TO   JOUTAI
*            引当情報_出荷予定数量
              MOVE   DEN-F15        TO   JOH-FB04
*            引当情報_消費税率
              MOVE   ZEIRITU        TO   JOH-FB06
*            引当情報_運送便コード
              MOVE   "SGWJP"        TO   JOH-FB07
*            引当情報_運送会社名
              MOVE   NC"佐川急便"   TO   JOH-FB08
*            付加情報_引当（日付）
              MOVE   SYS-DATEW      TO   JOH-FE09
*            付加情報_引当（時刻）
              MOVE   WK-TIME(1:6)   TO   JOH-FE10
*            付加情報_引当（部Ｃ）
              MOVE   PARA-IN-BUMCD  TO   JOH-FE11
*            付加情報_引当（担Ｃ）
              MOVE   PARA-IN-TANCD  TO   JOH-FE12
*            基本情報ファイル更新
              REWRITE  JOH-REC
     END-IF.
 MAIN010.
*    ＡＭＡＺＯＮ基本情報ファイル読込み
     PERFORM AMZJOHL2-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　ＡＭＡＺＯＮ出荷予定ワーク作成処理　　　　　　　　　　
****************************************************************
 AMZSYYWK-WRITE-SEC     SECTION.
*
     MOVE     "AMZSYYWK-WRITE-SEC"  TO   S-NAME.
*
     MOVE      SPACE                TO   SYY-REC.
     INITIALIZE                          SYY-REC.
*
*  共通情報_データ区分
     MOVE     "2"                   TO   SYY-F001.
*
*  受注情報フィールド
     MOVE      JOH-FA               TO   SYY-FA.
*
*  引当情報フィールド
     MOVE      JOH-FB               TO   SYY-FB.
*    引当情報_状態区分
*--- IF        DEN-F27D    =    0
*---           MOVE      "OS"       TO   SYY-FB03 JOUTAI
*--- END-IF.
*--- IF        DEN-F27D    =    1
*---      IF   DEN-F15     =   DEN-F50
*---           MOVE      "AC"       TO   SYY-FB03 JOUTAI
*---      ELSE
*---           MOVE      "AC"       TO   SYY-FB03 JOUTAI
*---      END-IF
*--- END-IF.
     IF        DEN-F15     =    0
               MOVE      "OS"       TO   SYY-FB03 JOUTAI
     ELSE
               MOVE      "AC"       TO   SYY-FB03 JOUTAI
     END-IF.
*
*    引当情報_出荷予定数量
     MOVE      DEN-F15              TO   SYY-FB04.
*    引当情報_消費税率
     INITIALIZE                          WK-ZEIHENKAN.
     MOVE     "0"                   TO   LINK-ZEIKBN.
     MOVE     JOH-FA04              TO   LINK-ZEIDATE.
     CALL     "SKYTAXPG"         USING   LINK-ZEIKBN
                                         LINK-ZEIDATE
                                         LINK-ZEIERR
                                         LINK-ZEIRITU
                                         LINK-DZEIRITU.
     IF     LINK-ZEIERR  NOT =  "0"
            MOVE      4000        TO   PROGRAM-STATUS
            DISPLAY NC"＃＃消費税率取得エラー＃＃"    UPON CONS
            DISPLAY NC"＃＃税区分" " = " LINK-ZEIKBN  UPON CONS
            DISPLAY NC"＃＃日付　" " = " LINK-ZEIDATE UPON CONS
            STOP  RUN
     ELSE
            COMPUTE  LINK-ZEIRITU-H  =  LINK-ZEIRITU  / 100
            COMPUTE  LINK-DZEIRITU-H =  LINK-DZEIRITU / 100
     END-IF.
     IF     LINK-ZEIERR  =  "1"
            MOVE  LINK-DZEIRITU-H   TO   SYY-FB06 ZEIRITU
     ELSE
            MOVE  LINK-ZEIRITU-H    TO   SYY-FB06 ZEIRITU
     END-IF.
*    引当情報_運送便コード
     MOVE     "SGWJP"               TO   SYY-FB07.
*    引当情報_運送会社名
     MOVE     NC"佐川急便"          TO   SYY-FB08.
*
*  検品実績フィールド
     MOVE      JOH-FC               TO   SYY-FC.
*
*  予備フィールド
     MOVE      JOH-FD               TO   SYY-FD.
*
     WRITE     SYY-REC.
     ADD       1                   TO  KOS1-CNT.
*    ADD       1                   TO  KMS-CNT.
*
 AMZSYYWK-WRITE-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY "AMAZONｷﾎﾝｼﾞｮｳﾎｳ READ CNT = " READ-CNT  UPON CONS.
     DISPLAY "ｳﾘｱｹﾞDT ﾅｼ      SKIP CNT = " SKIP2-CNT UPON CONS.
     DISPLAY "AMAZONｼｮｯｶﾖﾃｲDT WRT  CNT = " KOS1-CNT  UPON CONS.
*
     MOVE      KOS1-CNT  TO        PARA-OUT-KENSU.
*
*    CLOSE     SHTDENL1  AMZJOHL2  AMZSYYWK  JYOKEN1.
     CLOSE     SHTDENL1  AMZJOHL2  AMZSYYWK.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
