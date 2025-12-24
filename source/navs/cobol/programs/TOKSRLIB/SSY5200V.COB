****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ新ＥＤＩ　　　　　　　　　　*
*    業務名　　　　　　　：　ナフコ出荷支援　　　　　　　　　　*
*    モジュール名　　　　：　発注確認　ＣＳＶデータ作成　　　  *
*    作成日　　　　　　　：　2019/02/21                        *
*    作成者　　　　　　　：　INOUE                             *
*    処理概要　　　　　　：　発注確認店舗ワーク・発注確認商品　*
*                            明細ワークからＣＳＶデータを出力　*
*                            する。　　　　　　　              *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY5200V.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2019/02/21.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*発注確認店舗ワーク
     SELECT   MEITXXX1  ASSIGN    TO        DA-01-VI-MEITXXX1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       HTP-F01
                                            HTP-F09
                                            HTP-F06
                                            HTP-F07
                        FILE      STATUS    HTP-STATUS.
*発注確認ＣＳＶ（店舗）
     SELECT   NCSTXXXF  ASSIGN    TO        DA-01-NCSTXXXF
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    CTP-STATUS.
*発注確認商品明細ワーク
     SELECT   MEISXXX1  ASSIGN    TO        DA-01-VI-MEISXXX1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       HSM-F01   HSM-F06
                                            HSM-F08
                        FILE      STATUS    HSM-STATUS.
*発注確認ＣＳＶ（明細）
     SELECT   NCSMXXXF  ASSIGN    TO        DA-01-NCSMXXXF
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    CSM-STATUS.
*ナフコ商品マスタ
     SELECT   NFSHOMS1  ASSIGN     TO      DA-01-VI-NFSHOMS1
                        ORGANIZATION       INDEXED
                        ACCESS     MODE    RANDOM
                        RECORD     KEY     SHO-F01
                        FILE    STATUS     SHO-STATUS.
*ナフコ店舗マスタ
     SELECT   NFTENMS1  ASSIGN     TO      DA-01-VI-NFTENMS1
                        ORGANIZATION       INDEXED
                        ACCESS     MODE    RANDOM
                        RECORD     KEY     TEN-F01  TEN-F02
                        FILE    STATUS     TEN-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*発注確認店舗ワーク
******************************************************************
 FD  MEITXXX1
                        LABEL RECORD   IS   STANDARD.
     COPY     MEITXXX1  OF        XFDLIB
              JOINING   HTP  AS   PREFIX.
*
******************************************************************
*発注確認ＣＳＶ（店舗）
******************************************************************
 FD  NCSTXXXF            LABEL RECORD   IS   STANDARD
     BLOCK    CONTAINS   20        RECORDS.
     COPY     NCSTXXXF   OF        XFDLIB
              JOINING    CTP       PREFIX.
******************************************************************
*発注確認商品明細ワーク
******************************************************************
 FD  MEISXXX1
                        LABEL RECORD   IS   STANDARD.
     COPY     MEISXXX1  OF        XFDLIB
              JOINING   HSM  AS   PREFIX.
*
******************************************************************
*発注確認ＣＳＶ（明細）
******************************************************************
 FD  NCSMXXXF            LABEL RECORD   IS   STANDARD
     BLOCK    CONTAINS   1         RECORDS.
     COPY     NCSMXXX1   OF        XFDLIB
              JOINING    CSM       PREFIX.
******************************************************************
*ナフコ商品マスタ
******************************************************************
 FD  NFSHOMS1
                        LABEL RECORD   IS   STANDARD.
     COPY     NFSHOMS1  OF        XFDLIB
              JOINING   SHO  AS   PREFIX.
******************************************************************
*ナフコ店舗マスタ
******************************************************************
 FD  NFTENMS1
                        LABEL RECORD   IS   STANDARD.
     COPY     NFTENMS1  OF        XFDLIB
              JOINING   TEN  AS   PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*
*ＣＳＶ（商品明細１行目）
     COPY     NCSMXXX1   OF        XFDLIB
              JOINING    CS1       PREFIX.
*ＣＳＶ（商品明細２行目）
     COPY     NCSMXXX2   OF        XFDLIB
              JOINING    CS2       PREFIX.
*
*ＳＴＡＴＵＳ領域
 01  WK-ST.
     03  HTP-STATUS          PIC  X(02).
     03  HSM-STATUS          PIC  X(02).
     03  CTP-STATUS          PIC  X(02).
     03  CSM-STATUS          PIC  X(02).
     03  SHO-STATUS          PIC  X(02).
     03  TEN-STATUS          PIC  X(02).
*フラグ領域
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  MEITXXX1-END-FLG        PIC  X(03)     VALUE  SPACE.
 01  MEISXXX1-END-FLG        PIC  X(03)     VALUE  SPACE.
 01  NFTENMS1-INV-FLG        PIC  X(03)     VALUE  SPACE.
*カウンター領域
 01  WK-CNT.
     03  RD-CNT              PIC  9(08)     VALUE  ZERO.
     03  RD-CNT2             PIC  9(08)     VALUE  ZERO.
     03  OUT-CNT             PIC  9(08)     VALUE  ZERO.
     03  OUT-CNT2            PIC  9(08)     VALUE  ZERO.
*ブレイク領域
 01  WK-HTP-F01              PIC  9(08)     VALUE  ZERO.
 01  WK-HTP-F05              PIC  X(02)     VALUE  SPACE.
 01  WK-HTP-F09              PIC  9(08)     VALUE  ZERO.
*添字
 01  IX                      PIC  9(03)     VALUE  ZERO.
*システム日付編集領域
 01  WK-AREA.
     03  SYS-DATE            PIC  9(06).
     03  SYS-DATEW           PIC  9(08).
*メッセージ出力領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "SSY5200V".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SSY5200V".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SSY5200V".
         05  FILLER          PIC  X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  AB-FILE         PIC  X(08).
         05  FILLER          PIC  X(06)  VALUE " ST = ".
         05  AB-STS          PIC  X(02).
         05  FILLER          PIC  X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  FILLER          PIC  X(07)  VALUE " SEC = ".
         05  S-NAME          PIC  X(30).
     03  MSG-IN.
         05  FILLER          PIC  X(02)  VALUE "##".
         05  FILLER          PIC  X(24)  VALUE
                             " 店舗情報読込件数   = ".
         05  MSG-IN01        PIC  ZZ,ZZ9.
         05  FILLER          PIC  X(06)  VALUE
                             " 件 ".
         05  FILLER          PIC  X(01)  VALUE "#".
     03  MSG-OUT.
         05  FILLER          PIC  X(02)  VALUE "##".
         05  FILLER          PIC  X(24)  VALUE
                             " CSVデータ抽出件数  = ".
         05  MSG-OUT01       PIC  ZZ,ZZ9.
         05  FILLER          PIC  X(06)  VALUE
                             " 件 ".
         05  FILLER          PIC  X(01)  VALUE "#".
     03  MSG-IN2.
         05  FILLER          PIC  X(02)  VALUE "##".
         05  FILLER          PIC  X(24)  VALUE
                             " 明細情報読込件数   = ".
         05  MSG-IN02        PIC  ZZ,ZZ9.
         05  FILLER          PIC  X(06)  VALUE
                             " 件 ".
         05  FILLER          PIC  X(01)  VALUE "#".
     03  MSG-OUT2.
         05  FILLER          PIC  X(02)  VALUE "##".
         05  FILLER          PIC  X(24)  VALUE
                             " CSVデータ抽出件数  = ".
         05  MSG-OUT02       PIC  ZZ,ZZ9.
         05  FILLER          PIC  X(06)  VALUE
                             " 件 ".
         05  FILLER          PIC  X(01)  VALUE "#".
*ブレイクＫＥＹ領域
*01  WK-BREAK-KEY.
*    03  WK-K26              PIC  X(01)   VALUE  SPACE.
*日付変換サブルーチン用領域
 01  LINK-AREA.
     03  LINK-IN-KBN         PIC   X(01).
     03  LINK-IN-YMD6        PIC   9(06).
     03  LINK-IN-YMD8        PIC   9(08).
     03  LINK-OUT-RET        PIC   X(01).
     03  LINK-OUT-YMD8       PIC   9(08).
*
*LINKAGE                SECTION.
*01  PARA-KNOST              PIC   9(08).
*01  PARA-KNOED              PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
*PROCEDURE              DIVISION USING PARA-KNOST
*                                      PARA-KNOED.
 PROCEDURE              DIVISION.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   MEITXXX1.
     MOVE      "MEITXXX1"   TO   AB-FILE.
     MOVE      HTP-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NCSTXXXF.
     MOVE      "NCSTXXXF"   TO   AB-FILE.
     MOVE      CTP-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   MEISXXX1.
     MOVE      "MEISXXX1"   TO   AB-FILE.
     MOVE      HSM-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NCSMXXXF.
     MOVE      "NCSMXXXF"   TO   AB-FILE.
     MOVE      CSM-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFSHOMS1.
     MOVE      "NFSHOMS1"   TO   AB-FILE.
     MOVE      SHO-STATUS   TO   AB-STS.
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
*
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FLG   =  "END".
     PERFORM  END-SEC.
     STOP  RUN.
*
 GENERAL-PROCESS-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     MEITXXX1  MEISXXX1  NFSHOMS1  NFTENMS1.
     OPEN     OUTPUT    NCSTXXXF  NCSMXXXF.
     DISPLAY  MSG-START UPON CONS.
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
*発注確認店舗ワーク読込
     PERFORM MEITXXX1-READ-SEC.
*終了判定
     IF   MEITXXX1-END-FLG  =  "END"
          DISPLAY "＃＃出力対象店舗無し＃＃" UPON CONS
          MOVE    "END"     TO    END-FLG
          MOVE     4010     TO    PROGRAM-STATUS
     ELSE
          MOVE    HTP-F01     TO WK-HTP-F01
***       MOVE    HTP-F05     TO WK-HTP-F05
          MOVE    HTP-F09     TO WK-HTP-F09
     END-IF.
*
*発注確認商品明細ワーク読込
     PERFORM MEISXXX1-READ-SEC.
*終了判定
     IF   MEISXXX1-END-FLG  =  "END"
          DISPLAY "＃＃出力対象明細無し＃＃" UPON CONS
          MOVE    "END"     TO    END-FLG
          MOVE     4010     TO    PROGRAM-STATUS
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　発注確認店舗ワーク読込
****************************************************************
 MEITXXX1-READ-SEC    SECTION.
*
     MOVE    "MEITXXX1-READ-SEC"    TO   S-NAME.
*
     READ     MEITXXX1
              AT  END
                  MOVE     "END"    TO  MEITXXX1-END-FLG
                  GO                TO  MEITXXX1-READ-EXIT
     END-READ.
*
     ADD      1     TO     RD-CNT.
*
 MEITXXX1-READ-EXIT.
     EXIT.
****************************************************************
*　　発注確認商品明細ワーク読込
****************************************************************
 MEISXXX1-READ-SEC    SECTION.
*
     MOVE    "MEISXXX1-READ-SEC"    TO   S-NAME.
*
     READ     MEISXXX1
              AT  END
                  MOVE     "END"    TO  MEISXXX1-END-FLG
                  GO                TO  MEISXXX1-READ-EXIT
     END-READ.
*
     ADD      1     TO     RD-CNT2.
*
 MEISXXX1-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*
 MAIN-01.
     MOVE     SPACE          TO      CTP-REC.
*****INITIALIZE                      CTP-REC.
*
     PERFORM  VARYING  IX FROM 1 BY 1 UNTIL
            ( MEITXXX1-END-FLG    =   "END" )
        OR  ( IX          > 500        )
*             発注確認ＣＳＶ（店舗）SET
              PERFORM  NCSTXXXF-SET-SEC
*             発注確認店舗ワーク読込み
              PERFORM  MEITXXX1-READ-SEC
     END-PERFORM.
*
 MAIN-02.
     MOVE     SPACE          TO      CSM-REC.
*****INITIALIZE                      CSM-REC.
*    発注確認ＣＳＶ（明細１行目) SET
     PERFORM  NCSMXXXF-SET1-SEC.
*    発注確認ＣＳＶ（明細１行目) OUT
     MOVE     CS1-REC  TO CSM-REC.
     WRITE    CSM-REC.
     ADD      1        TO OUT-CNT2
*
*    発注確認ＣＳＶ（明細２行目) SET
     PERFORM  NCSMXXXF-SET2-SEC.
*    発注確認ＣＳＶ（明細２行目) OUT
     MOVE     CS2-REC  TO CSM-REC.
     WRITE    CSM-REC.
     ADD      1        TO OUT-CNT2
*
*    発注確認商品明細ワーク読込み
     PERFORM  MEISXXX1-READ-SEC.
     IF       MEISXXX1-END-FLG    =   "END"
              MOVE    "END"       TO   END-FLG
              GO                  TO   MAIN-EXIT
     ELSE
              GO                  TO   MAIN-02
     END-IF.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*発注確認ＣＳＶ（店舗）SET
****************************************************************
 NCSTXXXF-SET-SEC     SECTION.
*
     MOVE     "NCSTXXXF-SET-SEC"  TO  S-NAME.
*
 NCSTXXXF-SET-01.
*****初期化
     MOVE    SPACE     TO   CTP-REC.
     INITIALIZE             CTP-REC.
*****管理番号
     MOVE    HTP-F01   TO   CTP-F01.
     MOVE    ","       TO   CTP-A01.
*****作場ＣＤ
***  MOVE    HTP-F05   TO   CTP-F02.
***  MOVE    ","       TO   CTP-A02.
*****店着日
     MOVE    HTP-F09   TO   CTP-F02.
     MOVE    ","       TO   CTP-A02.
*****店舗ＣＤ
     MOVE    HTP-F06   TO   CTP-F03.
     MOVE    ","       TO   CTP-A03.
*****店舗マスタ索引
     MOVE    137607    TO   TEN-F01.
     MOVE    HTP-F06   TO   TEN-F02.
     PERFORM NFTENMS1-READ-SEC.
     IF  NFTENMS1-INV-FLG = "INV"
         DISPLAY NC"＃店舗Ｍ異常（未存在）" " TENPO-CD = "
         HTP-F06 NC"　＃" UPON CONS
         MOVE 4000     TO   PROGRAM-STATUS
         STOP RUN
     END-IF.
*****店舗名
     MOVE    X"28"     TO   CTP-F041.
     MOVE    TEN-F05   TO   CTP-F04.
     MOVE    X"29"     TO   CTP-F042.
     MOVE    ","       TO   CTP-A04.
*****郵便番号
     MOVE    TEN-F07   TO   CTP-F05.
     MOVE    ","       TO   CTP-A05.
*****住所
     MOVE    X"28"     TO   CTP-F061.
     MOVE    TEN-F08   TO   CTP-F06(1:15).
     MOVE    TEN-F09   TO   CTP-F06(16:15).
     MOVE    X"29"     TO   CTP-F062.
     MOVE    ","       TO   CTP-A06.
*****電話番号
     MOVE    TEN-F10   TO   CTP-F07.
     MOVE    ","       TO   CTP-A07.
*****県ＣＤ
     MOVE    TEN-F12   TO   CTP-F08.
***  MOVE    ","       TO   CTP-A08.
*****新発注確認ＣＳＶデータ（店舗）出力
     WRITE   CTP-REC.
     ADD     1         TO   OUT-CNT.
*
 NCSTXXXF-SET-EXIT.
     EXIT.
****************************************************************
*発注確認ＣＳＶ（明細１行目)SET
****************************************************************
 NCSMXXXF-SET1-SEC     SECTION.
*
     MOVE     "NCSMXXXF-SET1-SEC"  TO  S-NAME.
*
 NCSMXXXF-SET1-01.
     MOVE    SPACE     TO   CS1-REC.
*****管理番号
     MOVE    HSM-F01   TO   CS1-F01.
     MOVE    ","       TO   CS1-A01.
*****作場ＣＤ
***  MOVE    HSM-F05   TO   CS1-F02.
***  MOVE    ","       TO   CS1-A02.
*****店着日
     MOVE    HSM-F06   TO   CS1-F02.
     MOVE    ","       TO   CS1-A02.
*****相手商品ＣＤ
     MOVE    HSM-F08   TO   CS1-F04.
     MOVE    ","       TO   CS1-A04.
*    商品名,規格
     MOVE   X"28"      TO   CS1-F051.
     MOVE    HSM-F08     TO         SHO-F01.
     READ    NFSHOMS1
        INVALID
             MOVE      ALL NC"＊" TO   CS1-F05
             MOVE      ALL NC"＊" TO   CS1-F06
             MOVE      ALL "*"    TO   CS1-F03
             MOVE      ZERO       TO   CS1-F07
        NOT INVALID
             MOVE      SHO-F05    TO   CS1-F05
             MOVE      SHO-F06    TO   CS1-F06
             MOVE      SHO-F04    TO   CS1-F03
             MOVE      SHO-F09    TO   CS1-F07
     END-READ.
     MOVE   X"29"      TO   CS1-F052.
     MOVE    ","       TO   CS1-A05    CS1-A06    CS1-A07.
     MOVE    ","       TO   CS1-A03.
     MOVE   X"28"      TO   CS1-F061.
     MOVE   X"29"      TO   CS1-F062.
*
     PERFORM  VARYING  IX FROM 1 BY 1 UNTIL
              IX          > 500
              IF      HSM-F201(IX)  NOT = ZERO
                      MOVE  HSM-F201(IX) TO   CS1-F081(IX)
              END-IF
              MOVE    ","           TO   CS1-A08(IX)
     END-PERFORM.
*
 NCSMXXXF-SET1-EXIT.
     EXIT.
****************************************************************
*発注確認ＣＳＶ（明細２行目)SET
****************************************************************
 NCSMXXXF-SET2-SEC     SECTION.
*
     MOVE     "NCSMXXXF-SET2-SEC"  TO  S-NAME.
*
 NCSMXXXF-SET2-01.
     MOVE    SPACE     TO   CS2-REC.
*****管理番号
*    MOVE    HSM-F01   TO   CS2-F01.
     MOVE    ","       TO   CS2-A01.
*****作場ＣＤ
***  MOVE    HSM-F05   TO   CS2-F02.
***  MOVE    ","       TO   CS2-A02.
*****店着日
*    MOVE    HSM-F06   TO   CS2-F02.
     MOVE    ","       TO   CS2-A02.
*****相手商品ＣＤ
*    MOVE    HSM-F08   TO   CS2-F04.
     MOVE    ","       TO   CS2-A04.
*    商品名,規格
     MOVE   X"28"      TO   CS2-F051.
     MOVE   ALL NC"　" TO   CS2-F05.
     MOVE   ALL NC"　" TO   CS2-F06.
*    MOVE    HSM-F08     TO         SHO-F01.
*    READ    NFSHOMS1
*       INVALID
*            MOVE      ALL NC"＊" TO   CS2-F05
*            MOVE      ALL NC"＊" TO   CS2-F06
*            MOVE      ALL "*"    TO   CS2-F03
*            MOVE      ZERO       TO   CS2-F07
*       NOT INVALID
*            MOVE      SHO-F05    TO   CS2-F05
*            MOVE      SHO-F06    TO   CS2-F06
*            MOVE      SHO-F04    TO   CS2-F03
*            MOVE      SHO-F09    TO   CS2-F07
*    END-READ.
     MOVE   X"29"      TO   CS2-F052.
     MOVE    ","       TO   CS2-A05    CS2-A06    CS2-A07.
     MOVE    ","       TO   CS2-A03.
     MOVE   X"28"      TO   CS2-F061.
     MOVE   X"29"      TO   CS2-F062.
*
     PERFORM  VARYING  IX FROM 1 BY 1 UNTIL
              IX          > 500
              IF      HSM-F203(IX)  NOT = SPACE
                      MOVE  HSM-F203(IX) TO   CS2-F081(IX)
              END-IF
              MOVE    ","           TO   CS2-A08(IX)
     END-PERFORM.
*
 NCSMXXXF-SET2-EXIT.
     EXIT.
****************************************************************
*　　ナフコ店舗マスタ読込　　　　　　　　　　　　　　　　　　　*
****************************************************************
 NFTENMS1-READ-SEC    SECTION.
*
     READ  NFTENMS1
           INVALID      MOVE  "INV"   TO  NFTENMS1-INV-FLG
           NOT  INVALID MOVE  SPACE   TO  NFTENMS1-INV-FLG
     END-READ.
*
 NFTENMS1-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     MOVE     RD-CNT     TO      MSG-IN01.
     MOVE     RD-CNT2    TO      MSG-IN02.
     MOVE     OUT-CNT    TO      MSG-OUT01.
     MOVE     OUT-CNT2   TO      MSG-OUT02.
     DISPLAY  MSG-IN     UPON    CONS.
     DISPLAY  MSG-OUT    UPON    CONS.
     DISPLAY  MSG-IN2    UPON    CONS.
     DISPLAY  MSG-OUT2   UPON    CONS.
*
     CLOSE     NFSHOMS1  NFTENMS1.
     CLOSE     MEITXXX1  NCSTXXXF.
     CLOSE     MEISXXX1  NCSMXXXF.
*
     DISPLAY  MSG-END UPON CONS.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
