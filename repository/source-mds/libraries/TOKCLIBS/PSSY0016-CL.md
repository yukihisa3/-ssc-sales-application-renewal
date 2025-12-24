# PSSY0016

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/PSSY0016.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    出荷管理（流通ＢＭＳ）               *  ./
/. *   JOB-ID      :    PSSY0016                             *  ./
/. *   JOB-NAME    :    手書伝票データ抽出                   *  ./
/. *               :    13/03/22 バロー植物追加              *  ./
/. ***********************************************************  ./
    PGM
/.###ﾜｰｸｴﾘｱ定義####./
  /.VAR ?FILNM    ,STRING*8,VALUE-'        ' ./ /.ﾌｧｲﾙ名合併用./
  /.VAR ?FILNMH   ,STRING*6,VALUE-'JHTDEN'   ./ /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
  /.VAR ?FILNMT   ,STRING*5,VALUE-'JHTTE'    ./ /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
  /.VAR ?LIBNM    ,STRING*8,VALUE-'TOKFLIB ' ./ /.ﾗｲﾌﾞﾗﾘ名ﾃﾞﾌｫﾙﾄ./
  /.VAR ?FILID    ,NAME                      ./ /.ﾌｧｲﾙ名名前型./
  /.VAR ?LIBID    ,NAME                      ./ /.ﾗｲﾌﾞﾗﾘ名名前型./
  /.VAR ?JHTDENF  ,NAME!MOD                  ./ /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
  /.VAR ?JHTTEGF  ,NAME!MOD                  ./ /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
  /.VAR ?JHTDENFN ,STRING*17                 ./ /.ﾌｧｲﾙ名表示用./
  /.VAR ?JHTTEGFN ,STRING*17                 ./ /.ﾌｧｲﾙ名表示用./
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSSY0016'
    VAR       ?STEP     ,STRING*8
    VAR       ?P1       ,STRING*02,VALUE-'00'
    VAR       ?P2       ,STRING*08,VALUE-'00000000'
    VAR       ?P3       ,STRING*09,VALUE-'000000000'
    VAR       ?P4       ,STRING*09,VALUE-'000000000'
    VAR       ?P5       ,STRING*08,VALUE-'00000000'
    VAR       ?P6       ,STRING*09,VALUE-'000000000'
    VAR       ?P7       ,STRING*09,VALUE-'000000000'
    VAR       ?P8       ,STRING*08,VALUE-'00000000'
    VAR       ?P9       ,STRING*09,VALUE-'000000000'
    VAR       ?P10      ,STRING*09,VALUE-'000000000'
    VAR       ?P11      ,STRING*08,VALUE-'00000000'
    VAR       ?P12      ,STRING*09,VALUE-'000000000'
    VAR       ?P13      ,STRING*09,VALUE-'000000000'
    VAR       ?P14      ,STRING*08,VALUE-'00000000'
    VAR       ?P15      ,STRING*09,VALUE-'000000000'
    VAR       ?P16      ,STRING*09,VALUE-'000000000'
    VAR       ?P17      ,STRING*08,VALUE-'00000000'
    VAR       ?P18      ,STRING*09,VALUE-'000000000'
    VAR       ?P19      ,STRING*09,VALUE-'000000000'
    VAR       ?P20      ,STRING*08,VALUE-'00000000'
    VAR       ?P21      ,STRING*09,VALUE-'000000000'
    VAR       ?P22      ,STRING*09,VALUE-'000000000'
    VAR       ?P23      ,STRING*08,VALUE-'00000000'
    VAR       ?P24      ,STRING*09,VALUE-'000000000'
    VAR       ?P25      ,STRING*09,VALUE-'000000000'
    VAR       ?P26      ,STRING*08,VALUE-'00000000'
    VAR       ?P27      ,STRING*09,VALUE-'000000000'
    VAR       ?P28      ,STRING*09,VALUE-'000000000'
    VAR       ?P29      ,STRING*08,VALUE-'00000000'
    VAR       ?P30      ,STRING*09,VALUE-'000000000'
    VAR       ?P31      ,STRING*09,VALUE-'000000000'
    VAR       ?P32      ,STRING*08,VALUE-'00000000'
    VAR       ?P33      ,STRING*09,VALUE-'000000000'
    VAR       ?P34      ,STRING*09,VALUE-'000000000'
    VAR       ?P35      ,STRING*08,VALUE-'00000000'
    VAR       ?P36      ,STRING*09,VALUE-'000000000'
    VAR       ?P37      ,STRING*09,VALUE-'000000000'
    VAR       ?P38      ,STRING*02,VALUE-'00'
    VAR       ?OPR1   ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2   ,STRING*50                  /.      2    ./
    VAR       ?OPR3   ,STRING*50                  /.      3    ./
    VAR       ?OPR4   ,STRING*50                  /.      4    ./
    VAR       ?OPR5   ,STRING*50                  /.      5    ./
/.##DEFLIBL##./
    DEFLIBL BMSFLIB/TOKELIB/TOKFLIB
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##倉庫ｺｰﾄﾞ取得##./
SKY1601B:

    ?STEP :=   'SKY1601B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?P1,?P38)
    IF        @PGMEC    ^=   0   THEN
              GOTO ABEND
    END

/.##手書伝票データ抽出指示入力##./
PSSY0016:

    ?STEP :=   'PSSY0016'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    DEFLIBL   TOKELIB/TOKFLIB

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    CALL      PGM-SSY0015I.TOKELIB,PARA-(?P1,
              ?P2,?P3,?P4,?P5,?P6,?P7,?P8,?P9,?P10,
              ?P11,?P12,?P13,?P14,?P15,?P16,?P17,?P18,?P19,
              ?P20,?P21,?P22,?P23,?P24,?P25,?P26,?P27,?P28,
              ?P29,?P30,?P31,?P32,?P33,?P34,?P35,?P36,?P37,?P38)
    IF        @PGMEC    ^=   0    THEN
         IF   @PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO RTN
         ELSE
              GOTO ABEND
         END
    END

/.##ﾌｧｲﾙ名称取得##./
/. ------------------------
FILNMCHG:

    ?MSGX := '## 実行倉庫ｺｰﾄﾞ = ' && ?P1 && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
                             --------------------------- ./

 /. ?FILNM    :=    ?FILNMH && ?P1      .//.ﾃﾞﾌｫﾙﾄﾌｧｲﾙ名+倉庫CD./
 /. ?FILID    :=    %NAME(?FILNM)       .//.ﾌｧｲﾙ名名前型変換   ./
 /. ?LIBID    :=    %NAME(?LIBNM)       .//.ﾗｲﾌﾞﾗﾘ名名前型変換 ./
 /. ?JHTDENF  :=    %NCAT(?FILID,?LIBID).//.ﾌｧｲﾙ名.ﾗｲﾌﾞﾗﾘ名    ./
 /. ?JHTDENFN :=    %STRING(?JHTDENF)   ./
 /. ?MSGX     :=    '## 倉庫F名(SF) = ' && ?JHTDENFN && ' ##'./
 /. SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES            ./
 /. ?FILNM    :=    ?FILNMT && ?P1      .//.ﾃﾞﾌｫﾙﾄﾌｧｲﾙ名+倉庫CD./
 /. ?FILID    :=    %NAME(?FILNM)       .//.ﾌｧｲﾙ名名前型変換   ./
 /. ?LIBID    :=    %NAME(?LIBNM)       .//.ﾗｲﾌﾞﾗﾘ名名前型変換 ./
 /. ?JHTTEGF  :=    %NCAT(?FILID,?LIBID).//.ﾌｧｲﾙ名.ﾗｲﾌﾞﾗﾘ名    ./
 /. ?JHTTEGFN :=    %STRING(?JHTTEGF)   ./
 /. ?MSGX     :=    '## 倉庫F名(PF) = ' && ?JHTTEGFN && ' ##'./
 /. SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES            ./

/.##出力ファイル確認##./
PASSIGN:

    ?STEP :=   'PASSIGN '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

  /.ASSIGN FILE-?JHTDENF!@XCL    ./
    ASSIGN FILE-JHTTEGSF.BMSFLIB!@XCL
    IF     @PGMEC  ^=    0    THEN
           GOTO  CHK
    END

/.##手書伝票データ抽出##./
SSY0016B:

    ?STEP :=   'SSY0016B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SHTDENLC,TOFILE-SHTDENLC.TOKFLIB
  /.OVRF      FILE-JHTDENF,TOFILE-?JHTDENF  ./
    OVRF      FILE-JHTDENF,TOFILE-JHTTEGSF.BMSFLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    OVRF      FILE-SHOTBL2,TOFILE-SHOTBL2.TOKFLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    CALL      PGM-SSY0016B.TOKELIBO,PARA-(?P1,
              ?P2,?P3,?P4,?P5,?P6,?P7,?P8,?P9,?P10,
              ?P11,?P12,?P13,?P14,?P15,?P16,?P17,?P18,?P19,
              ?P20,?P21,?P22,?P23,?P24,?P25,?P26,?P27,?P28,
              ?P29,?P30,?P31,?P32,?P33,?P34,?P35,?P36,?P37)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END
    /. データＳＯＲＴ ./
    SORTF  INFILE-JHTTEGSF.BMSFLIB,
           OUTFILE-JHTTEGWF.BMSFLIB,
           KEY-'F01!A',
           SELECT-'F01!EQ!#00013020',
           RCDL-@DSP
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END
    ?MSGX :=  'SORTF-OUT  '
    SNDMSG    ?MSGX,TO-XCTL

/.##抽出対象の各出荷場所ﾌｧｲﾙへﾃﾞｰﾀをｺﾋﾟｰする##./
  /.SETPF FILE-?JHTDENF,TOFILE-?JHTTEGF,ADD-@NO,ACTCHK-@NO ./
    SETPF FILE-JHTTEGWF.BMSFLIB,TOFILE-BMSTEGF.BMSFLIB,
          ADD-@NO,ACTCHK-@NO
    /. データＳＯＲＴ ./
    SORTF  INFILE-JHTTEGSF.BMSFLIB,
           OUTFILE-JHTTEGWF.BMSFLIB,
           KEY-'F01!A',
           SELECT-'F01!EQ!#00013052',
           RCDL-@DSP
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END
    ?MSGX :=  'SORTF-OUT  '
    SNDMSG    ?MSGX,TO-XCTL

/.##抽出対象の各出荷場所ﾌｧｲﾙへﾃﾞｰﾀをｺﾋﾟｰする##./
    SETPF FILE-JHTTEGWF.BMSFLIB,TOFILE-BMSTE2F.BMSFLIB,
          ADD-@NO,ACTCHK-@NO

    /. データＳＯＲＴ ./
    SORTF  INFILE-JHTTEGSF.BMSFLIB,
           OUTFILE-JHTTEGWF.BMSFLIB,
           KEY-'F01!A',
           SELECT-'F01!EQ!#00751225',
           RCDL-@DSP
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END
    ?MSGX :=  'SORTF-OUT  '
    SNDMSG    ?MSGX,TO-XCTL

/.##抽出対象の各出荷場所ﾌｧｲﾙへﾃﾞｰﾀをｺﾋﾟｰする##./
    SETPF FILE-JHTTEGWF.BMSFLIB,TOFILE-BMSTE7F.BMSFLIB,
          ADD-@NO,ACTCHK-@NO

    /. データＳＯＲＴ ./
    SORTF  INFILE-JHTTEGSF.BMSFLIB,
           OUTFILE-JHTTEGWF.BMSFLIB,
           KEY-'F01!A',
           SELECT-'F01!EQ!#00781225',
           RCDL-@DSP
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END
    ?MSGX :=  'SORTF-OUT  '
    SNDMSG    ?MSGX,TO-XCTL

/.##抽出対象の各出荷場所ﾌｧｲﾙへﾃﾞｰﾀをｺﾋﾟｰする##./
    SETPF FILE-JHTTEGWF.BMSFLIB,TOFILE-BMSTE8F.BMSFLIB,
          ADD-@NO,ACTCHK-@NO

    /. データＳＯＲＴ ./
    SORTF  INFILE-JHTTEGSF.BMSFLIB,
           OUTFILE-JHTTEGWF.BMSFLIB,
           KEY-'F01!A',
           SELECT-'F01!EQ!#00761995',
           RCDL-@DSP
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END
    ?MSGX :=  'SORTF-OUT  '
    SNDMSG    ?MSGX,TO-XCTL

/.##抽出対象の各出荷場所ﾌｧｲﾙへﾃﾞｰﾀをｺﾋﾟｰする##./
    SETPF FILE-JHTTEGWF.BMSFLIB,TOFILE-BMSTE9F.BMSFLIB,
          ADD-@NO,ACTCHK-@NO

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC


ABEND:

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=    '### ' && ?PGMID && ' ABEND' && ' ###'
    ?MSG(2)   :=    '### ' && ' PGMEC = ' &&
                     %SBSTR(?PGMECX,8,4) && ' ###'
    ?MSG(3)   :=    '###' && ' LINE = '  && %LAST(LINE)      && ' ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL
    END

    RETURN    PGMEC-@PGMEC
/.#################################################################./
/.##資源使用時、他端末開放確認メッセージ出力）                   ##./
/.#################################################################./
CHK:  /.資源の解放./

    ?OPR1  :=  '　　＃＃＃＃＃＃＃　資源使用中　＃＃＃＃＃＃＃　　'
    ?OPR2  :=  '　　　　　　　　　　　　　　　　　　　　　　　　　'
    ?OPR3  :=  '　　他端末にて資源を使用中です。　　　　　　　　　'
    ?OPR4  :=  '　　全端末の使用状況を確認して下さい。　　　　　　'
    ?OPR5  :=  '　ＥＮＴＥＲでリスタート，ＰＦ９でプログラム終了　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

    GOTO      PASSIGN

```
